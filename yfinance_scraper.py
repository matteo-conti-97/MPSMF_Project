import yfinance as yf
import ast 
import pandas as pd

if __name__ == '__main__':
    
    european = ["^SPX", "^NDX", "^DJX"]  #NASDAQ NDX O IXIC? Perché il secondo non mi da le opzioni, idem per dow jones DJX o DJI il secondo non da opzioni
    
    for idx in european:
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
            call = opt.calls
            all_calls = pd.concat([all_calls, call], ignore_index=True)
            put = opt.puts
            all_puts = pd.concat([all_puts, put], ignore_index=True)
        
        all_calls.to_csv('./data/'+ idx + '_calls.csv', index=False) 
        all_puts.to_csv('./data/' + idx + '_puts.csv', index=False) 
        