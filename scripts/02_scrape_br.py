'''Scrape basketball-reference.com

Scrape basketball-reference.com to get data on
advanced stats, season honors, and series outcomes
for players in replication data from Biegert et al. (2023).

HTML pages are cached by default, so subsequent runs won't
have to fetch from basketball-reference.com.
'''

import csv
import requests
import time
import sys
from collections import defaultdict
from bs4 import BeautifulSoup, Comment
from pathlib import Path
import re

cache_dir = Path('data/brcache')
cache_dir.mkdir(parents = True, exist_ok = True)

###
# enumerate stats to collect by html class
# (relevant stats will be collected for both 
# regular season and playoffs)
###

# player advanced stats
# (season, team_id and height are automatically included)
advanced_stat_labels = [
    'pos','age','g','bpm','obpm','dbpm'
]
# game log stats
# (player_id, season, minutes_played (mp) are automatically included)
game_log_stat_labels = [
    'date_game','team_id','opp_id', 'game_result',
    'pts','ast','trb','blk','stl','fga','fta','tov'
]



####
# scraping functions
####

def fetch_player_stats(player_id):
    '''Scrape individual player's stats, returning a dict
    with {season: {stat_label: stat_value}}'''

    res = defaultdict(dict)

    # download page or get cached data
    cpath = Path(f'{cache_dir}/players/{player_id}.html')
    cpath.parent.mkdir(parents = True, exist_ok = True)
    if cpath.exists():
        with open(cpath,'rt') as f:
            soup = BeautifulSoup(f.read(),features="lxml")
    else:
        url = f'https://www.basketball-reference.com/players/{player_id[0]}/{player_id}.html'
        while True:
            response = requests.get(url)
            if response.ok:
                break
            else:
                print(f"retrying {url}")
                time.sleep(2)
        with open(cpath,'wb') as f:
            f.write(response.content)
        soup = BeautifulSoup(response.content,features="lxml")
        time.sleep(3) # Crawl-delay: 3 from robots.txt

    # a cludgy way to get height from the raw source in cm:
    height = int(re.findall(r'\((\d\d\d)cm\,', str(soup))[0])

    # row selectors for regular season and playoffs tables
    selectors = {
        ('rs', '#advanced tbody tr'),
        ('po', '#playoffs_advanced tbody tr')
    }

    # regular season advanced stats
    for rowlabel, selector in selectors:
        for row in soup.select(selector):
            season = int(row['id'].split('.')[1])
            res[season]['height'] = height
            stats = {td['data-stat']:td.get_text() for td in row.select('td')}
            if res[season].get(f'team_id_{rowlabel}','') == 'TOT':
                # don't overwrite season totals
                continue
            res[season][f'team_id_{rowlabel}'] = stats['team_id']
            for stat_name in advanced_stat_labels:
                try:
                    res[season][f'{stat_name}_{rowlabel}'] = float(stats[stat_name])
                except ValueError:
                    res[season][f'{stat_name}_{rowlabel}'] = stats[stat_name]
                except KeyError:
                    pass

    return(res)

def fetch_player_games(player_id, season):
    '''Scrape individual player's game stats for a given season, 
    returning a list of tuples'''
    res = list()

    # download page or get cached data
    cpath = Path(f'{cache_dir}/game_logs/{player_id}_{season}.html')
    cpath.parent.mkdir(parents = True, exist_ok = True)
    if cpath.exists():
        with open(cpath,'rt') as f:
            soup = BeautifulSoup(f.read(),features="lxml")
    else:
        url = f'https://www.basketball-reference.com/players/{player_id[0]}/{player_id}/gamelog/{year}'
        while True:
            response = requests.get(url)
            if response.ok:
                break
            else:
                print(f"retrying {url}")
                time.sleep(2)
        with open(cpath,'wb') as f:
            f.write(response.content)
        soup = BeautifulSoup(response.content,features="lxml")
        time.sleep(3) # Crawl-delay: 3 from robots.txt

    # the playoffs table (if present) is embedded as an html comment
    # so we parse this as a separate document
    try:
        soup_po = BeautifulSoup(soup.select('#all_pgl_basic_playoffs')[0].find_all(string=lambda text: isinstance(text, Comment))[0],features="lxml")
    except IndexError:
        soup_po = None

    # the selectors need to point to the document to look in (soup).
    selectors = [
        ('rs', soup, '#pgl_basic tbody tr:not(.thead)'),
        ('po', soup_po, '#pgl_basic_playoffs tbody tr:not(.thead)')
    ]

    for _, sp, selector in selectors:
        if sp is None:
            continue
        for row in sp.select(selector):
            rowres = {}
            stats = {td['data-stat']:td.get_text() for td in row.select('td')}
            # parse minutes played from mm:ss to float
            if 'mp' in stats and len(stats['mp'].strip()) > 0:
                minutes, seconds = [float(v) for v in stats['mp'].split(':')]
                rowres['mp'] = minutes + seconds/60
            else:
                rowres['mp'] = None
            for stat_name in game_log_stat_labels:
                if stat_name not in stats or len(stats[stat_name].strip()) == 0:
                    # no useable value
                    rowres[stat_name] = None
                    continue
                try:
                    # try to convert to float
                    rowres[stat_name] = float(stats[stat_name])
                except ValueError:
                    # otherwise keep as str
                    rowres[stat_name] = stats[stat_name]
            res.append(
                (player_id, season, rowres['mp']) +  tuple([rowres[k] for k in game_log_stat_labels])
            )

    return(res)



def fetch_season_selections(season):
    '''A bunch of stats at the season level.
    Returns player_res and team_res'''
    player_res = defaultdict(lambda: defaultdict(lambda:0))
    team_res = defaultdict(lambda: defaultdict(lambda:0))

    # download page or get cached data
    cpath = Path(f'{cache_dir}/seasons/{season}.html')
    cpath.parent.mkdir(parents = True, exist_ok = True)
    if cpath.exists():
        with open(cpath,'rt') as f:
            soup = BeautifulSoup(f.read(),features="lxml")
    else:
        url = f'https://www.basketball-reference.com/leagues/NBA_{season}.html'
        while True:
            response = requests.get(url)
            if response.ok:
                break
            else:
                print(f"retrying {url}")
                time.sleep(2)
        with open(cpath,'wb') as f:
            f.write(response.content)
        soup = BeautifulSoup(response.content,features="lxml")
        time.sleep(3) # Crawl-delay: 3 from robots.txt

    # all-Star
    try:
        all_star_soup = BeautifulSoup(soup.select('#all_all_star_game_rosters')[0].find_all(string=lambda text: isinstance(text, Comment))[0],features="lxml")
        for hon_div in all_star_soup.select(".data_grid_box"):
            team_num = int(hon_div['id'].split('_')[-1])
            for a in hon_div.select('a'):
                player_id = a['href'].split('/')[-1].replace('.html','')
                player_res[player_id]['all_star'] = team_num
    except IndexError:
        pass 

    # all-NBA
    try:
        all_nba_soup = BeautifulSoup(soup.select('#all_all-nba')[0].find_all(string=lambda text: isinstance(text, Comment))[0],features="lxml")
        for hon_div in all_nba_soup.select(".data_grid_box"):
            team_num = int(hon_div['id'].split('_')[-1])
            for a in hon_div.select('a'):
                player_id = a['href'].split('/')[-1].replace('.html','')
                player_res[player_id]['all_nba'] = team_num
    except IndexError:
        pass 

    # all-defensive
    try:
        all_defensive_soup = BeautifulSoup(soup.select('#all_all-defensive')[0].find_all(string=lambda text: isinstance(text, Comment))[0],features="lxml")
        for hon_div in all_defensive_soup.select(".data_grid_box"):
            team_num = int(hon_div['id'].split('_')[-1])
            for a in hon_div.select('a'):
                player_id = a['href'].split('/')[-1].replace('.html','')
                player_res[player_id]['all_defensive'] = team_num
    except IndexError:
        pass 

    # playoff standings
    try:
        playoff_soup = BeautifulSoup(soup.select('#all_all_playoffs')[0].find_all(string=lambda text: isinstance(text, Comment))[0],features="lxml")
        for row in playoff_soup.select('tbody tr'):
            if row.has_attr('class'):
                # this is a game row, skip it
                continue
            cells = row.select('td')
            series = cells[0].get_text()
            teams = [a['href'].split('/')[-2] for a in cells[1].select('a')]
            if series == 'Finals':
                team_res[teams[0]]['champions'] = 1
                team_res[teams[0]]['finals'] = 1
                team_res[teams[1]]['finals'] = 1
            elif series == 'Eastern Conference Finals':
                team_res[teams[0]]['finals_eastern'] = 1
                team_res[teams[1]]['finals_eastern'] = 1
            elif series == 'Western Conference Finals':
                team_res[teams[0]]['finals_western'] = 1
                team_res[teams[1]]['finals_western'] = 1
            elif series == 'Eastern Conference Semifinals':
                team_res[teams[0]]['semifinals_eastern'] = 1
                team_res[teams[1]]['semifinals_eastern'] = 1
            elif series == 'Western Conference Semifinals':
                team_res[teams[0]]['semifinals_western'] = 1
                team_res[teams[1]]['semifinals_western'] = 1
            elif series in ('Eastern Conference First Round', "Western Conference First Round"):
                team_res[teams[0]]['playoffs'] = 1
                team_res[teams[1]]['playoffs'] = 1
    except IndexError:
        pass 


    return(player_res, team_res)



if __name__ == '__main__':
    in_path = 'data/andata.csv'
    player_data_path = 'data/player_data.csv'
    game_data_path = 'data/game_data.csv'

    min_season = 1983 
    max_season = 2016

    #####
    # Player-level table
    #####

    # get players
    start_season = {}
    with open(in_path,'rt') as f:
        reader = csv.reader(f)
        header = next(reader)
        for row in reader:
            season = int(row[9])
            mseason = start_season.get(row[0], 3000)
            start_season[row[0]] = min(season, mseason)
    # we only want players who's career started in [min_season, max_season]
    all_players = set([p for p, y in start_season.items() if y >= min_season and y <= max_season])

    # fetch all the player stats
    player_stats = dict()
    print('fetching player logs:')
    for i,player_id in enumerate(all_players):
        sys.stdout.write(f'\r{i+1}/{len(all_players)}: {player_id}        ')
        sys.stdout.flush()
        player_stats[player_id] = fetch_player_stats(player_id)
    sys.stdout.write('\n')

    # fetch all the season stats
    all_seasons = set()
    for player_id, ystats in player_stats.items():
        all_seasons.update(ystats.keys())
    season_stats_player = dict()
    season_stats_team = dict()
    print('\nfetching season logs:')
    for i,season in enumerate(all_seasons):
        if season < min_season or season > max_season:
            continue
        sys.stdout.write(f'\r{season}')
        sys.stdout.flush()
        stats_player, stats_team = fetch_season_selections(season)
        season_stats_player[season] = stats_player
        season_stats_team[season] = stats_team
    sys.stdout.write('\n')

    # construct the output header in pieces
    out_header = ('player_id','season','team_id_rs','height') #base
    out_header += tuple([f'{stat}_rs' for stat in advanced_stat_labels]) # adv rs
    out_header += tuple([f'{stat}_po' for stat in advanced_stat_labels]) # adv po
    out_header += ('all_star','all_nba','all_defensive','mvp','dpoy') # player season
    out_header += ('champions','finals','finals_eastern','finals_western','semifinals_eastern','semifinals_western', 'playoffs') # team season

    # assemble output rows
    out_rows = []
    for player_id, ystats in player_stats.items():
        for season,stats in ystats.items():
            if season < min_season or season > max_season:
                continue
            ###
            # Base stats
            ###
            row = [player_id, season, stats.get('team_id_rs', None), stats.get('height', None)]

            ###
            # Advanced stats (rs)
            ###
            for stat in [f'{s}_rs' for s in advanced_stat_labels]:
                row.append(stats.get(stat,None))

            ###
            # Advanced stats (po)
            ###
            for stat in [f'{s}_po' for s in advanced_stat_labels]:
                row.append(stats.get(stat,None))
            
            ###
            # awards
            ###
            row.append(season_stats_player[season][player_id]['all_star'])
            row.append(season_stats_player[season][player_id]['all_nba'])
            row.append(season_stats_player[season][player_id]['all_defensive'])
            row.append(season_stats_player[season][player_id]['mvp'])
            row.append(season_stats_player[season][player_id]['dpoy'])
            
            ###
            # series
            ###
            team_id_po = stats.get('team_id_po',None)
            for game in ['champions','finals','finals_eastern','finals_western','semifinals_eastern','semifinals_western','playoffs']:
                row.append(season_stats_team[season][team_id_po][game])
            
            # append
            out_rows.append(row)
    with open(player_data_path,'wt') as f:
        writer = csv.writer(f)
        writer.writerow(out_header)
        writer.writerows(out_rows)



    #####
    # Game-level table
    #####

    # fetch all the game logs
    player_seasons = []
    for player_id, stats in player_stats.items():
        for season in stats.keys():
            if season > max_season:
                continue
            player_seasons.append((player_id, season))
    game_logs = list()
    print('\nfetching game logs:')
    for i, (player_id, season) in enumerate(player_seasons):
        if type(season) is not int:
            continue
        sys.stdout.write(f'\r{i+1}/{len(player_seasons)}: {player_id} {season} logs        ')
        sys.stdout.flush()
        game_logs.extend(fetch_player_games(player_id, season))
    sys.stdout.write('\n')

    game_header = ('player_id', 'season', 'mp') +  tuple(game_log_stat_labels)
    with open(game_data_path,'wt') as f:
        writer = csv.writer(f)
        writer.writerow(game_header)
        writer.writerows(game_logs)
