import yfinance as yf
import ast 
import pandas as pd
from datetime import datetime
import os


def scrape_options_data(options, today):
    
    for idx in options:
        spx = yf.Ticker(idx)

        # get option chain for specific expiration
        try:
            opt = spx.option_chain('0000-00-00')
        except Exception as e:
            list_string = "[" + str(e).split('[')[1]
            list_string = list_string.replace(" ", "")
            list_string = list_string.replace(",", "','")
            list_string = list_string.replace("[", "['")
            list_string = list_string.replace("]", "']")
            option_dates = ast.literal_eval(list_string)
        
        all_calls = pd.DataFrame()
        all_puts = pd.DataFrame()
        
        # Define the cutoff date
        cutoff_date = datetime(2024, 12, 31)
        
        for date in option_dates:
            # Convert date to a datetime object if it's not already one
            if isinstance(date, str):
                date_obj = datetime.strptime(date, '%Y-%m-%d')
            
            if date_obj < cutoff_date:
                opt = spx.option_chain(date)
                
                #Process calls
                call = opt.calls
                call['expiration_date'] = date #add expiration date to the dataframe
                all_calls = pd.concat([all_calls, call], ignore_index=True)
                #all_calls = all_calls[all_calls.isna().sum(axis=1) <= 1]
                #all_calls = all_calls.dropna()
                
                #Process puts
                put = opt.puts
                put['expiration_date'] = date #add expiration_date to the dataframe
                all_puts = pd.concat([all_puts, put], ignore_index=True)
                #all_puts = all_puts[all_puts.isna().sum(axis=1) <= 1]
                #all_puts = all_puts.dropna()
        
        #If doesn't exist, create a data folder
        all_calls.to_csv('./data/raw/' + today + '/' + today + '_' + idx + '_calls.csv', index=False)
        all_puts.to_csv('./data/raw/' + today + '/' + today + '_' + idx + '_puts.csv', index=False)
        


if __name__ == '__main__':
        
    european = ['^SPX', '^NDX', '^RUT']
    #european = ['^NDX']
    
    american = ['NVDA', 'JNJ', 'XOM']
    
    #Get today date in format yyyy_mm_dd
    today = pd.Timestamp.today().strftime('%Y_%m_%d')
    
    try:
        os.makedirs('./data/raw/' + today)
    except Exception as e:
        print('Data already written for today')
        exit()
    
    print('Scraping European options data')
    scrape_options_data(european, today)
    
    print('Scraping American options data')
    scrape_options_data(american, today)
        
    print('Scraping completed')
    