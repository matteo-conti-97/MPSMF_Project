# Autore: Simone Nicosanti
#
# I file csv di output sono inseriti nella directory OutputDirectory; nello specifico viene creato:
#   - Un file di output per ogni giorno nell'intervallo temporale specificato se specificato True nel campo generate
#   - un file di output complessivo con tutti i dati dell'intervallo
#
# Ad ogni esecuzione la directory di output viene eliminata e sostituita con quella dell'esecuzione attuale.
# Viene aggiunta anche una colonna Date al dataset per indicare la data a cui i dati si riferiscono.
#
# NOTA. Librerie aggiuntive necessarie per l'esecuzione:
#   - requests : "pip3 install requests"
#   - pandas : "pip3 install pandas"
#

import pandas as pd
import requests
import datetime
from io import StringIO
import os
import shutil

BASE_HTTP = "https://www.treasurydirect.gov/GA-FI/FedInvest/securityPriceDetail"
HEADER_ARRAY = ["Cusip", "SecurityType", "Rate", "MaturityDate", "CallDate", "Buy", "Sell", "EndOfDay", "Date"]
OUTPUT_DIR_NAME = "OutputDirectory"

def readDataSetWithInput() :
    firstDateString = input("Inserire prima data (dd-mm-yyyy) >>> ")
    lastDateString = input("Inserire ultima data (dd-mm-yyyy oppure T per data odierna) >>> ")
    generateString = input("Generare i file per ogni data?? (y-n) >>> ")

    firstDate = datetime.datetime.strptime(firstDateString, "%d-%m-%Y").date()
    if (lastDateString == "T") :
        lastDate = datetime.date.today()
    else :
        lastDate = datetime.datetime.strptime(lastDateString, "%d-%m-%Y").date()
    generate = (generateString == "y")

    readDataSet(firstDate, lastDate, generate)


def readDataSet(firstDate, lastDate = datetime.date.today(), generate = False) :
    # @Param firstDate : datetime
    # @Param lastDate : datetime
    # @Param generate : Boolean
    # @return dataset : DataFrame

    initEnv()

    if (lastDate > datetime.date.today() or firstDate > datetime.date.today()) :
        print("DATE NON VALIDE: Devono essere minori o uguali alla data odierna")
        return

    dataset = pd.DataFrame(columns = HEADER_ARRAY)

    currentDate = firstDate
    while (currentDate <= lastDate) :
        dateDataFrame = retrieveDataForDate(currentDate, dataset)

        if (generate) :
            dateDataFrame.to_csv(os.path.join(OUTPUT_DIR_NAME, str(currentDate) + "_output.csv"), index = False, float_format = "%.6f")

        dataset = pd.concat([dataset, dateDataFrame], ignore_index = True)

        currentDate = currentDate + datetime.timedelta(days = 1)

    print("Scrittura CSV")
    dataset.to_csv(
        os.path.join(OUTPUT_DIR_NAME, "TotalOutput_" + str(firstDate) + "_" + str(lastDate) + ".csv"), 
        index = False, 
        float_format = "%.6f")

    input("Premere Invio per Terminare...")

    return dataset


def initEnv() :
    if (os.path.isdir(OUTPUT_DIR_NAME)) :
        shutil.rmtree(OUTPUT_DIR_NAME)
    os.mkdir(OUTPUT_DIR_NAME)


def retrieveDataForDate(currentDate, dataset) :
    # @param currentDate : datetime
    # @param dataset : DataFrame
    # @return newDataset : DataFrame

    print("Recupero per data: ", currentDate)
    csvString = csvRetrieve(currentDate)
    dateDataFrame = parseCsvString(csvString, dataset, currentDate)
    return dateDataFrame


def csvRetrieve(currentDate) :
    # @param currentDate : datetime
    # @return csvText : String
    month = currentDate.month
    day = currentDate.day
    year = currentDate.year

    postParams = {"priceDateDay" : str(day), "priceDateMonth" : str(month), "priceDateYear" : str(year), "fileType" : "csv", "csv" : "CSV+FORMAT"}

    response = requests.post(BASE_HTTP, data = postParams)
    return response.text


def parseCsvString(csvString, dataset, currentDate) :
    # @param csvString : String
    # @param dataset : DataFrame
    # @param currentDate : datetime
    # @return newDataset : DataFrame

    dateDataFrame = pd.read_csv(StringIO(csvString), header = None, sep = ",", names = HEADER_ARRAY)
    dateDataFrame["Date"] = currentDate
    dateDataFrame["Date"] = pd.to_datetime(dateDataFrame["Date"].astype(str))
    dateDataFrame["MaturityDate"] = pd.to_datetime(dateDataFrame['MaturityDate'].astype(str), format = "%m/%d/%Y")

    return dateDataFrame


if __name__ == "__main__" :
    readDataSetWithInput()
