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
        
            
        

def get_equal_maturities_list(option1, option2):
    
    calls = pd.read_csv('./data/'+ option1 + '_calls.csv')
    maturities1 = calls['maturity'].unique()
    
    calls = pd.read_csv('./data/'+ option2 + '_calls.csv')
    maturities2 = calls['maturity'].unique()
    
    #Find the common maturities
    common_maturities = []
    for maturity1 in maturities1:
        for maturity2 in maturities2:
            if maturity1 == maturity2:
                common_maturities.append(maturity1)
    
    return common_maturities

def filter_maturities(option, maturities):
    calls = pd.read_csv('./data/'+ option + '_calls.csv')
    puts = pd.read_csv('./data/'+ option + '_puts.csv')
    
    calls = calls[calls['maturity'].isin(maturities)]
    puts = puts[puts['maturity'].isin(maturities)]
    
    calls.to_csv('./data/'+ option + '_calls_same_maturity.csv', index=False) 
    puts.to_csv('./data/' + option + '_puts_same_maturity.csv', index=False)


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
    
    """ #Get common maturities
    common_maturities1 = get_equal_maturities_list(european[0], american[0])
    common_maturities2 = get_equal_maturities_list(european[0], american[1])
    common_maturities3 = get_equal_maturities_list(european[0], american[2])
    
    common_maturities4= get_equal_maturities_list(european[1], american[0])
    common_maturities5 = get_equal_maturities_list(european[1], american[1])
    common_maturities6 = get_equal_maturities_list(european[1], american[2])
    
    common_maturities7 = get_equal_maturities_list(european[2], american[0])
    common_maturities8 = get_equal_maturities_list(european[2], american[1])
    common_maturities9 = get_equal_maturities_list(european[2], american[2])
    
    #Get only common maturities accross all the common maturities lists
    common_maturities = list(set(common_maturities1) & set(common_maturities2) & set(common_maturities3) & 
                             set(common_maturities4) & set(common_maturities5) & set(common_maturities6) & 
                             set(common_maturities7) & set(common_maturities8) & set(common_maturities9))
    
    for option in european:
        filter_maturities(option, common_maturities)
    
    for option in american:
        filter_maturities(option, common_maturities) """