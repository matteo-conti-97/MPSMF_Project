import requests
from bs4 import BeautifulSoup
from datetime import datetime, timezone
import csv
import openpyxl

def convert_date_to_epoch(data_str, date_format='%Y-%m-%d'):
	dt=datetime.strptime(date_str,date_format)
	utc_dt = dt.replace(tzinfo=timezone.utc)
	epoch_timestamp=int(utc_dt.timestamp())
	return epoch_timestamp

def scrape_data(date_str, calls_or_puts):
	
	output_excel=f"yahoo_data_{calls_or_puts}_{date_str}.xlsx"

	epoch_timestamp=convert_date_to_epoch(date_str)

	#costruzione del'url riguardante le call o le puts
	url=f"https://finance.yahoo.com/quote/%5ESPX/options/?date={epoch_timestamp}&type={calls_or_puts}"

	try:
		headers={'User-Agent':'Mozilla/5.0 (Windows NT 10.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36 Edge/12.10136'}		

		#get per ottenere il contenuto della pagina web
		response=requests.get(url, headers=headers)
		response.raise_for_status()

		soup=BeautifulSoup(response.content, "html.parser")

		table=soup.find("table")
	
		th=[header.text for header in table.find_all("th")]
		rows=[]
		for row in table.find_all("tr"):
			cells=row.find_all("td")
			rows.append([cell.text.strip() for cell in cells])

		wb=openpyxl.Workbook()
		ws=wb.active

		ws.append(th)

		for row in rows:
			ws.append(row)

		wb.save(output_excel)

		print(f"Dati salvati")

	except requests.RequestException as e:
		print(f"eccezione {e}")

def scrape_calls_data(data_str):
	scrape_data(date_str,"calls")

def scrape_puts_data(data_str):
	scrape_data(date_str,"puts")

date_str="2024-06-10"

scrape_calls_data(date_str)
scrape_puts_data(date_str)