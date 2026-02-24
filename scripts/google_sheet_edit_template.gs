/**
 * Google Apps Script template for editing a specific Google Sheet tab by gid.
 *
 * Target spreadsheet:
 * https://docs.google.com/spreadsheets/d/1CAFM0UHwShVwRUHQzJGa-gluk6qjIW4cP4Lo7q--TrY/edit#gid=1125770648
 *
 * How to use:
 * 1) In Google Sheets, open Extensions -> Apps Script.
 * 2) Paste this file into the editor (or create a standalone script).
 * 3) Edit the `edits` array in `main()` to match the changes you want.
 * 4) Run `main()` and authorize access when prompted.
 */

const SPREADSHEET_ID = '1CAFM0UHwShVwRUHQzJGa-gluk6qjIW4cP4Lo7q--TrY';
const TARGET_GID = 1125770648;

function main() {
  const sheet = getSheetByGid_(SPREADSHEET_ID, TARGET_GID);

  // Replace these examples with the edits you want to apply.
  const edits = [
    {
      type: 'setValues',
      range: 'A1:C2',
      values: [
        ['Header 1', 'Header 2', 'Header 3'],
        ['Value A', 'Value B', 'Value C'],
      ],
    },
    {
      type: 'appendRows',
      rows: [
        ['New row 1 col A', 'New row 1 col B', 'New row 1 col C'],
        ['New row 2 col A', 'New row 2 col B', 'New row 2 col C'],
      ],
    },
    {
      type: 'updateRowsByKey',
      keyColumnName: 'ID', // header text in row 1
      updates: [
        { key: '123', values: { Status: 'Complete', Notes: 'Updated by script' } },
        { key: '456', values: { Status: 'Pending' } },
      ],
    },
  ];

  applyEdits_(sheet, edits);
}

function applyEdits_(sheet, edits) {
  edits.forEach((edit) => {
    switch (edit.type) {
      case 'setValues':
        applySetValues_(sheet, edit);
        break;
      case 'appendRows':
        applyAppendRows_(sheet, edit);
        break;
      case 'updateRowsByKey':
        applyUpdateRowsByKey_(sheet, edit);
        break;
      default:
        throw new Error(`Unsupported edit type: ${edit.type}`);
    }
  });
}

function applySetValues_(sheet, edit) {
  if (!edit.range || !Array.isArray(edit.values) || edit.values.length === 0) {
    throw new Error('setValues requires `range` and non-empty `values`.');
  }

  const range = sheet.getRange(edit.range);
  const numRows = edit.values.length;
  const numCols = edit.values[0].length;

  if (range.getNumRows() !== numRows || range.getNumColumns() !== numCols) {
    throw new Error(
      `Range ${edit.range} size (${range.getNumRows()}x${range.getNumColumns()}) ` +
        `does not match values size (${numRows}x${numCols}).`
    );
  }

  range.setValues(edit.values);
}

function applyAppendRows_(sheet, edit) {
  if (!Array.isArray(edit.rows) || edit.rows.length === 0) {
    throw new Error('appendRows requires non-empty `rows`.');
  }

  const width = edit.rows[0].length;
  const invalid = edit.rows.findIndex((row) => row.length !== width);
  if (invalid !== -1) {
    throw new Error(`appendRows row ${invalid} has inconsistent column count.`);
  }

  const startRow = sheet.getLastRow() + 1;
  sheet.getRange(startRow, 1, edit.rows.length, width).setValues(edit.rows);
}

function applyUpdateRowsByKey_(sheet, edit) {
  if (!edit.keyColumnName || !Array.isArray(edit.updates) || edit.updates.length === 0) {
    throw new Error('updateRowsByKey requires `keyColumnName` and non-empty `updates`.');
  }

  const data = sheet.getDataRange().getValues();
  if (data.length < 1) {
    throw new Error('Sheet is empty.');
  }

  const headers = data[0];
  const keyColIndex = headers.indexOf(edit.keyColumnName);
  if (keyColIndex === -1) {
    throw new Error(`Header not found: ${edit.keyColumnName}`);
  }

  const headerIndex = new Map(headers.map((h, i) => [String(h), i]));
  const rowIndexByKey = new Map();

  for (let r = 1; r < data.length; r += 1) {
    rowIndexByKey.set(String(data[r][keyColIndex]), r);
  }

  edit.updates.forEach((update) => {
    const rowIndex = rowIndexByKey.get(String(update.key));
    if (rowIndex === undefined) {
      throw new Error(`No row found for key: ${update.key}`);
    }
    Object.entries(update.values || {}).forEach(([columnName, value]) => {
      const colIndex = headerIndex.get(columnName);
      if (colIndex === undefined) {
        throw new Error(`Header not found: ${columnName}`);
      }
      data[rowIndex][colIndex] = value;
    });
  });

  sheet.getDataRange().setValues(data);
}

function getSheetByGid_(spreadsheetId, gid) {
  const spreadsheet = SpreadsheetApp.openById(spreadsheetId);
  const sheet = spreadsheet.getSheets().find((s) => s.getSheetId() === gid);
  if (!sheet) {
    throw new Error(`No sheet found for gid=${gid}`);
  }
  return sheet;
}
