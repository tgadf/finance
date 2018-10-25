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
from strdate import getDate, getYearMonth
from convertBase import showSummary, createJSON, writeCSVFile, initCSVoutput
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
    if row.get('Date'):
        output['date']=getDate(row['Date'])
        

    # Amount
    if row.get("Interest"):
        output['amount']=float(row['Interest'])
        output['type'] = "Interest"
    if row.get("Opening"):
        output['amount']=float(row['Opening'])
        output['type'] = "OpeningBalance"
    if row.get("Cont"):
        output['amount']=float(row['Cont'])
        output['type'] = "Contribution"


    # Payee
    output['payee'] = account


    # Statement
    output['statement']=getYearMonth(output['date'])


    # Original
    output['original']=output['type']

    # Category
    if output['type'] == "OpeningBalance":
        output['category'] = account+"OpeningBalance"
    if output['type'] == "Interest":
        output['category'] = account+"Interest"
    if output['type'] == "Contribution":
        output['category'] = account+"Contribution"
        

    output['transfer'] = "NONE"

    # Check for None
    for k,v in output.iteritems():
        if v == None:
            print "No",k," in row!"
            print "row ->",row
            print "out ->",output
            exit()

    return output


def createFidelityRuthCSV(basedir = '/Users/tgadfort/Documents/finance',
                    account = "FidelityRuth", debug = False):
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



def createFidelityRuthJSON(basedir = '/Users/tgadfort/Documents/finance',
                    account = "FidelityRuth", debug = False):
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)

def showFidelityRuthRecords(basedir = '/Users/tgadfort/Documents/finance',
                     account = "FidelityRuth", debug = False):
    showSummary(account, basedir=basedir, debug=debug, showYear = False)
    
def run(debug = False):
    createFidelityRuthCSV(debug=debug)
    createFidelityRuthJSON(debug=debug)


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

    account="FidelityRuth"
    if args.run:
        createFidelityRuthCSV(account=account, debug=args.debug)
        createFidelityRuthJSON(account=account, debug=args.debug)
    if args.csv:
        createFidelityRuthCSV(account=account, debug=args.debug)
    if args.json:
        createFidelityRuthJSON(account=account, debug=args.debug)
    if args.show:
        showFidelityRuthRecords(account=account, debug=args.debug)