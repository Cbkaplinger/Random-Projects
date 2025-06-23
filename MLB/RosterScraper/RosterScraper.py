def RosterScraper():
    import pandas as pd
    import numpy as np
    from bs4 import BeautifulSoup
    import requests
    import unicodedata

    TeamList = ["dbacks", "braves", "orioles", "redsox", "cubs", "whitesox", "reds", "guardians", "rockies", "tigers", "astros", "royals", "angels", "dodgers", "marlins", "brewers", "twins", "mets", "yankees", "athletics", "phillies", "pirates", "padres", "giants", "mariners", "cardinals", "rays", "rangers", "bluejays", "nationals"]
    Roster = pd.DataFrame(columns=['Name', "Position", 'Team'])

    for team in TeamList:
        url = f'https://www.mlb.com/{team}/roster/40-man'
        response = requests.get(url)
        soup = BeautifulSoup(response.content, 'html.parser')

        # Find all table rows
        rows = soup.find_all('tr')

        current_position = ""
        player_data = []

        for row in rows:
            # Check if this row is a position header
            position_header = row.find('td', colspan='2')
            if position_header:
                current_position = position_header.text.strip()
            else:
                # This is a player row
                info_cell = row.find('td', class_='info')
                if info_cell:
                    name_link = info_cell.find('a')
                    if name_link:
                        player_name = name_link.text.strip()
                        player_data.append({
                            'Name': player_name,
                            'Position': current_position,
                            'Team': team
                        })

        # Create a temporary DataFrame for the current team
        temp_df = pd.DataFrame(player_data)

        # Append the temporary DataFrame to the Roster DataFrame
        Roster = pd.concat([Roster, temp_df], ignore_index=True)

    def replace_special_chars(text):
        return unicodedata.normalize('NFKD', text).encode('ascii', 'ignore').decode('utf-8')
    
    def categorize_player(position):
        if position == "Pitchers":
            return "Pitcher"
        else:
            return "Batter"
        
    def convert_name(name):
        if name == 'rockies':
            return 'COL'
        elif name == 'reds':
            return 'CIN'
        elif name == 'mariners':
            return 'SEA'
        elif name == 'nationals':
            return 'WSH'
        elif name == 'yankees':
            return 'NYY'
        elif name == 'astros':
            return 'HOU'
        elif name == 'redsox':
            return 'BOS'
        elif name == 'athletics':
            return 'OAK'
        elif name == 'mets':
            return 'NYM'
        elif name == 'braves':
            return 'ATL'
        elif name == 'giants':
            return 'SF'
        elif name == 'brewers':
            return 'MIL'
        elif name == 'rays':
            return 'TB'
        elif name == 'royals':
            return 'KC'
        elif name == 'whitesox':
            return 'CWS'
        elif name == 'cubs':
            return 'CHC'
        elif name == 'angels':
            return 'LAA'
        elif name == 'tigers':
            return 'DET'
        elif name == 'dbacks':
            return 'ARI'
        elif name == 'guardians':
            return 'CLE'
        elif name == 'orioles':
            return 'BAL'
        elif name == 'twins':
            return 'MIN'
        elif name == 'marlins':
            return 'MIA'
        elif name == 'phillies':
            return 'PHI'
        elif name == 'rangers':
            return 'TEX'
        elif name == 'dodgers':
            return 'LAD'
        elif name == 'padres':
            return 'SD'
        elif name == 'pirates':
            return 'PIT'
        elif name == 'bluejays':
            return 'TOR'
        elif name == 'cardinals':
            return 'STL'
        else:
            return np.nan
        
    Roster["Name"] = Roster['Name'].apply(replace_special_chars)
    Roster['Position'] = Roster['Position'].apply(categorize_player)
    Roster['Team'] = Roster['Team'].apply(convert_name)

    return(Roster)