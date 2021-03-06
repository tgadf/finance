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
from show import sortRecords
from strdate import getDate, getYearMonth, getMDY
from convertBase import showSummary, createJSON, writeCSVFile, initCSVoutput, writeCSVFileFromDict
from edits import applyEdits


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
    if row.get('date'):
        output['date']=getDate(row['date'])
        mon,day,year     = getMDY(output['date'])
        output['month']  = int(mon)
        output['day']    = int(day)
        output['year']   = int(year)
        

    # Type
    if row.get('type'):
        output['type']=row['type']


    # Amount
    if row.get('amount'):
        output['amount']=float(row['amount'])


    # Payee
    output['payee'] = account
    if output['type'] == "Transfer":
        output['payee'] = "TIAAFNALThomas"


    # Statement
    output['statement']=getYearMonth(output['date'])


    # Original
    output['original']=output['type']


    ## Category
    if output['type'] == "OpeningBalance":
        output['category']=account+"OpeningBalance"
    if output['type'] == "Contribution":
        output['category']=account+"Contribution"
    if output['type'] == "Interest":
        output['category']=account+"Interest"
    if output['type'] == "Transfer":
        output['category']=account+"Transfer"


    # Transfer
    output['transfer'] = "NONE"
    if output['type'] == "Transfer":
        output['transfer'] = "INTO"
              
          
    # Check for None
    for k,v in output.iteritems():
        if v == None:
            print "No",k," in row!"
            print "row ->",row
            print "out ->",output
            raise()

    return output


def createFidelityFNALBalance(account = "FidelityFNAL", debug = False):
    basedir = getBaseLocation()
    fileio.removeSubPattern(basedir, [account, "original"], ".csv", debug)
    files = fileio.findSubExt(basedir, [account, "balance"], ".csv")
    for ifile in files:
        csvdata  = fileio.getCSV(ifile, delimiter=" ")
        data=[]
        nrows=0   
        pvalue = None
        for row in csvdata:
            data.append(row)        
        records = sortRecords(data)
        data = []
        for row in records:
            value = row['balance'].replace("$","")
            balance = float(value.replace(",",""))
            print ""
            print "--->",value
            if pvalue:
                print "\t--->",balance,'\t',pvalue
                value = balance - pvalue
            else:
                print "\t--->",balance,'\tNone'
                value = balance
            pvalue = balance
            descr = row['type']
            line={"date": row['date'], "amount": value, "type": descr}
            data.append(line)
            nrows += 1
        print "\tProcessed",len(data),"/",nrows,"rows in",ifile
        writeCSVFileFromDict(data, account=account, subdir="original", basedir=basedir, debug=debug)



def createFidelityFNALCSV(account = "FidelityFNAL", debug = False):
    basedir = getBaseLocation()
    fileio.removeSubPattern(basedir, [account, "csv"], ".csv", debug)
    files = fileio.findSubExt(basedir, [account, "original"], ".csv")
    for ifile in files:
        csvdata = fileio.get(ifile)
        data=[]
        nrows=0
        for row in csvdata:
            nrows += 1
            vals=parseRow(row, account)
            data.append(vals)
        print "\tProcessed",len(data),"/",nrows,"rows in",ifile
        data = applyEdits(data, debug=debug)
        writeCSVFile(data, basedir=basedir, account=account, debug=debug)



def createFidelityFNALJSON(account = "FidelityFNAL", debug = False):
    basedir = getBaseLocation()
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)



def showFidelityFNALRecords(account = "FidelityFNAL", debug = False):
    basedir = getBaseLocation()
    showSummary(account, basedir=basedir, debug=debug, showYear = False)
    
    
    
def run(debug = False):
    createFidelityFNALBalance(debug=debug)
    createFidelityFNALCSV(debug=debug)
    createFidelityFNALJSON(debug=debug)


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

    account="FidelityFNAL"
    if args.run:
        createFidelityFNALBalance(account=account, debug=args.debug)
        createFidelityFNALCSV(account=account, debug=args.debug)
        createFidelityFNALJSON(account=account, debug=args.debug)
    if args.csv:
        createFidelityFNALCSV(account=account, debug=args.debug)
    if args.json:
        createFidelityFNALJSON(account=account, debug=args.debug)
    if args.show:
        showFidelityFNALRecords(account=account, debug=args.debug)