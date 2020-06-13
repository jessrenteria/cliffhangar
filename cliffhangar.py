"""Scraper for ingesting Hangar 18 occupancy data into Google Sheets.
"""

from collections import defaultdict
from datetime import datetime
import json
import requests
import time
from typing import Any, Dict, List


from bs4 import BeautifulSoup
from google.oauth2 import service_account
from googleapiclient.discovery import build


# Direct link to the iframe on https://www.climbhangar18.com/
# which contains gym occupancy data.
OCCUPANCY_URL = 'https://portal.rockgympro.com/portal/public/74083a89f418928244e5479ea18be366/occupancy'

# OAuth credentials for a service account.
SERVICE_ACCOUNT_FILE = 'credentials.json'

# API scope for Google Sheets.
SCOPES = ['https://www.googleapis.com/auth/spreadsheets']

# Spreadsheet info.
SPREADSHEET_ID = '1F2RJlMYaliHg7U5mP4WZ721myGId8eDVev7X_GkP0pU'
RANGE = 'Occupancy Log!A2:U'


def format_scraped_json(data: str) -> str:
    """Formatting hacks to be able to parse the JSON from the script.
    """
    return '\n'.join(
            map(lambda x: x.strip().replace('\'', '"'),
                data.splitlines())).replace(',\n}', '\n}').replace(',    }', '}')


def scrape_occupancy_data(data: str) -> str:
    """Returns the first substring containing matching braces, or None.
    """
    start_index = None
    unclosed_braces = 0
    for index, char in enumerate(data):
        if char == '{':
            if start_index is None:
                start_index = index
            unclosed_braces += 1
        elif char == '}':
            if unclosed_braces == 0:
                return None
            unclosed_braces -= 1
            if unclosed_braces == 0:
                return format_scraped_json(data[start_index:index+1])
    return None


def json_to_row(data: str) -> List[str]:
    """Parses a JSON object and converts to a Google Sheets row.
    """
    values = defaultdict(lambda: defaultdict(str))
    values.update(json.loads(data))

    return [
        datetime.now().isoformat(' '),
        str(values['ARC']['count']),
        str(values['ARC']['capacity']),
        str(values['ERV']['count']),
        str(values['ERV']['capacity']),
        str(values['HDS']['count']),
        str(values['HDS']['capacity']),
        str(values['LON']['count']),
        str(values['LON']['capacity']),
        str(values['MVJ']['count']),
        str(values['MVJ']['capacity']),
        str(values['RCU']['count']),
        str(values['RCU']['capacity']),
        str(values['RIV']['count']),
        str(values['RIV']['capacity']),
        str(values['SCL']['count']),
        str(values['SCL']['capacity']),
        str(values['SPB']['count']),
        str(values['SPB']['capacity']),
        str(values['UPL']['count']),
        str(values['UPL']['capacity'])
    ]


def scrape_script(script: str) -> List[str]:
    """Scrapes Hangar 18 occupancy data contained in embedded JavaScript.

    The HTML contains a <script> that looks like this:

    ```
    <script>
    var data = {
      'UPL': {
        'capacity': 14
        'count': 5
        // ...
      },
      // ...
    };
    // ...
    </script>
    ```

    We can cheese the parsing of this by just finding the first opening brace
    and its matching closing brace and throwing that into a JSON parser.
    """
    return json_to_row(scrape_occupancy_data(script))


def scrape(url: str) -> List[str]:
    """Convenience wrapper.

    Fetches the HTML of the provided URL and does a hard-coded grab of the
    third <script> present on the page.

    Returns occupancy data formatted as a Google Sheet row.
    """
    soup = BeautifulSoup(requests.get(url).content, 'html.parser')
    return scrape_script(soup('script')[2].string)


def make_value_range(row: List[str]) -> Dict[str, Any]:
    """Convert raw row to a Google Sheets ValueRange.
    """
    return {
        'majorDimension': 'ROWS',
        'values': [row]
    }


def main():
    credentials = service_account.Credentials.from_service_account_file(
            SERVICE_ACCOUNT_FILE, scopes=SCOPES)
    service = build('sheets', 'v4', credentials=credentials)

    row = scrape(OCCUPANCY_URL)
    print(row)
    values = make_value_range(row)

    sheet = service.spreadsheets()
    sheet.values().append(spreadsheetId=SPREADSHEET_ID,
                          range=RANGE,
                          body=values,
                          valueInputOption='USER_ENTERED',
                          insertDataOption='INSERT_ROWS'
                          ).execute()


if __name__ == '__main__':
    main()
