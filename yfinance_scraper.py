import yfinance as yf
import ast 
import pandas as pd


def scrape_options_data(options):
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
        
        for date in option_dates:
            opt = spx.option_chain(date)
            
            #Process calls
            call = opt.calls
            call['maturity'] = date #add maturity date to the dataframe
            all_calls = pd.concat([all_calls, call], ignore_index=True)
            
            #Process puts
            put = opt.puts
            put['maturity'] = date #add maturity date to the dataframe
            all_puts = pd.concat([all_puts, put], ignore_index=True)
        
        #check if file already exists  
        
        try:    
            calls = pd.read_csv('./data/'+ idx + '_calls.csv')
        
            puts = pd.read_csv('./data/'+ idx + '_puts.csv')
            
            #Mantain only the rows in all_calls that are not in calls
            all_calls = all_calls[~all_calls.isin(calls)].dropna()
            
            #Mantain only the rows in all_puts that are not in puts
            all_puts = all_puts[~all_puts.isin(puts)].dropna()
            
            all_calls.to_csv('./data/'+ idx + '_calls.csv', mode='a', index=False)
            all_puts.to_csv('./data/' + idx + '_puts.csv', mode='a', index=False)    
        except:
            all_calls.to_csv('./data/'+ idx + '_calls.csv', index=False)
            all_puts.to_csv('./data/' + idx + '_puts.csv', index=False)    
        

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
    
    european = ['^SPX', '^NDX', '^DJX']  #NASDAQ NDX O IXIC? Perché il secondo non mi da le opzioni, idem per dow jones DJX o DJI il secondo non da opzioni
    
    american = ['NVDA', 'JNJ', 'XOM']
    
    print('Scraping European options data')
    scrape_options_data(european)
    
    print('Scraping American options data')
    scrape_options_data(american)
        
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