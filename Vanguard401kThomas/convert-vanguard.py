# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 10:39:07 2015

@author: tgadfort
"""

import sys
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')
    sys.path.insert(0, '/Users/tgadfort/Python')

import fileio
import argparse
from location import getBaseLocation
from strdate import getDate, getYearMonth, getMDY
from convertBase import showSummary, createJSON, writeCSVFile, initCSVoutput, writeCSVFileFromDict
from edits import applyEdits
from getter import sortRecords



    
##########################################
#
# Parse output from CSV to produce CSV
#
##########################################
def stripKeys(row):
    keys=row.keys()
    for k in keys:
        if k != k.strip():
            row[k.strip()] = row[k]
            del row[k]    
    return row

    
def parseRow(row, account):
    output=initCSVoutput()
    output['account'] = account

    row = stripKeys(row)


    # Date
    if row.get('Date'):
        output['date']=getDate(row['Date'])
        mon,day,year     = getMDY(output['date'])
        output['month']  = int(mon)
        output['day']    = int(day)
        output['year']   = int(year)


    # Action
    if row.get('Transaction Description'):
        output['type']=row['Transaction Description']


    # Price
    if row.get('Transaction Amount'):
        val=row['Transaction Amount']
        val=val.replace("$", "")
        val=val.replace(",","")
        val=val.replace("\xe2\x80\x93 ", "")
        output['amount']=float(val)


    # Quantity
    if row.get('Price'):
        val=row['Price']
        val=val.replace("$", "")
        val=val.replace(",","")
        val=val.replace("\xe2\x80\x93 ", "")
        output['price']=float(val)


    # Statement
    output['statement']=getYearMonth(output['date'])


    # Original
    output['original'] = output['type']


    # Category
    if output['type'].find("OpeningBalance") != -1:
        output['category'] = account+"OpeningBalance"
        output['payee'] = account             
    elif output['type'] == "Plan Contribution":
        output['category'] = account+"Contribution"
        output['payee'] = "AnthemPaycheck"    
    elif output['type'] == "Fee":
        output['category'] = account+"Charges"
        output['payee'] = account
    elif output['type'].find("Interest") != -1:
        output['category'] = account+"Interest"
        output['payee'] = account
    else:
        raise ValueError("Did not recognize type:",output['type'])


    # Type
    output['type'] = output['category'].replace(account, "")

    # Transfer
    output['transfer']="NONE"


    # Check for None
    for k,v in output.iteritems():
        if v == None:
            print "No",k," in row!"
            print "  row  ->",row
            print "  out  ->",output
            exit()

    return output



def createVanguard401kThomasCSV(account = "Vanguard401kThomas", debug = False):
    basedir = getBaseLocation()
    fileio.removeSubPattern(basedir, [account, "csv"], ".csv", debug)
    files = fileio.findSubExt(basedir, [account, "original"], ".csv")
    for ifile in files:
        csvdata  = fileio.getCSV(ifile, delimiter="\t")
        data=[]
        nrows=0
        for row in csvdata:
            nrows += 1
            vals=parseRow(row, account)
            data.append(vals)
        print "\tProcessed",len(data),"/",nrows,"rows in",ifile
        data = applyEdits(data, debug=debug)
        writeCSVFile(data, basedir=basedir, account=account, debug=debug)



def createVanguard401kThomasJSON(account = "Vanguard401kThomas", debug = False):
    basedir = getBaseLocation()
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)



def showVanguard401kThomasRecords(account = "Vanguard401kThomas", debug = False):
    basedir = getBaseLocation()
    showSummary(account, basedir=basedir, debug=debug, showYear = False)


    
def run(debug = False):
    createVanguard401kThomasCSV(debug=debug)
    createVanguard401kThomasJSON(debug=debug)


###############################################################################
#
# Main()
#
###############################################################################
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-run', action="store_true", dest='run',  default=False, help='Produce csv files and create JSON.')
    parser.add_argument('-csv', action="store_true", dest='csv',  default=False, help='Produce csv files from original.')
    parser.add_argument('-json',action="store_true", dest='json', default=False, help='Produce json files from the csv files.')
    parser.add_argument('-show',action="store_true", dest='show', default=False, help='Show records from json file.')
    parser.add_argument('-debug',action="store_true", dest='debug', default=False, help='Debug info.')
    args = parser.parse_args()

    account="Vanguard401kThomas"
    if args.run:
        createVanguard401kThomasCSV(account=account, debug=args.debug)
        createVanguard401kThomasJSON(account=account, debug=args.debug)
    if args.csv:
        createVanguard401kThomasCSV(account=account, debug=args.debug)
    if args.json:
        createVanguard401kThomasJSON(account=account, debug=args.debug)
    if args.show:
        showVanguard401kThomasRecords(account=account, debug=args.debug)