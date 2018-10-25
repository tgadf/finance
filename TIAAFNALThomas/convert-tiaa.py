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
def parseRow(row, account, source):
    output=initCSVoutput()
    output['account'] = account


    # Date
    if row.get('Date'):
        output['date']=getDate(row['Date'])


    # Amount
    if row.get('Amount'):
        output['amount']=float(row['Amount'])


    # Memo
    if row.get('Memo'):
        output['type']=row['Memo']


    # Statement
    output['statement']=getYearMonth(output['date'])


    # Original
    output['original']=output['type']
    

    # Category
    if output['type'] == "Opening Balance":
        output['category'] = account+"OpeningBalance"
    if output['type'] == "Dividends":
        output['category'] = account+"Interest"
    if output['type'] == "Short-term capital gains":
        output['category'] = account+"Interest"
    if output['type'] == "Long-term capital gains":
        output['category'] = account+"Interest"
    if output['type'] == "Contribution":
        output['category'] = account+"Contribution"
    if output['type'] == "Plan Servicing Credit":
        output['category'] = account+"Interest"
    if output['type'] == "Direct transfer to other company":
        output['category'] = account+"Transfer"
    if output['type'] == "Transfer":
        output['category'] = account+"Conversion"
        

    output['type']=None
    if output['category'].find("Gains") != -1:
        output['type']="Interest"
    if output['category'].find("Dividends") != -1:
        output['type']="Interest"
    if output['category'].find("Interest") != -1:
        output['type']="Interest"
    if output['category'].find("OpeningBalance") != -1:
        output['type']="OpeningBalance"
    if output['category'].find("Credit") != -1:
        output['type']="Deposit"
    if output['category'].find("Contribution") != -1:
        output['type']="Deposit"
    if output['category'].find("Transfer") != -1:
        output['type']="Transfer"
    if output['category'].find("Conversion") != -1:
        output['type']="Conversion"
    if output['type'] == None:
        raise ValueError("No type for",output['category'])
        

    # Payee
    if output['type'] == account+"Transfer":
        output['payee'] = "FidelityFNAL"
    else:
        output['payee']= account


    if output['type'] == account+"Transfer":
        output['transfer'] = "OUT"
    elif output['type'] == account+"Conversion":
        if output['amount'] > 0:
            output['transfer'] = "INTO"
        else:
            output['transfer'] = "OUT"
    else:
        output['transfer'] = "NONE"


    # Check for None
    for k,v in output.iteritems():
        if v == None:
            print "No",k," in row!"
            print "row ->",row
            print "out ->",output
            raise()

    return output


def createTIAAFNALThomasCSV(basedir = '/Users/tgadfort/Documents/finance',
                    account = "TIAAFNALThomas", debug = False):
    fileio.removeSubPattern(basedir, [account, "csv"], ".csv", debug)
    files = fileio.findSubExt(basedir, [account, "original"], ".csv")
    for ifile in files:
        csvdata  = fileio.get(ifile)
        basename = fileio.getBasename(ifile)
        source   = basename.split('.')[1].title()
        data=[]
        nrows=0
        for row in csvdata:
            nrows += 1
            vals=parseRow(row, account, source)
            data.append(vals)
        print "\tProcessed",len(data),"/",nrows,"rows in",ifile
        data = applyEdits(data, debug=debug)
        writeCSVFile(data, basedir=basedir, account=account, debug=debug)



def createTIAAFNALThomasJSON(basedir = '/Users/tgadfort/Documents/finance',
                    account = "TIAAFNALThomas", debug = False):
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)

def showTIAAFNALThomasRecords(basedir = '/Users/tgadfort/Documents/finance',
                     account = "TIAAFNALThomas", debug = False):
    showSummary(account, basedir=basedir, debug=debug, showYear = False)
    
def run(debug = False):
    createTIAAFNALThomasCSV(debug=debug)
    createTIAAFNALThomasJSON(debug=debug)


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

    account="TIAAFNALThomas"
    if args.run:
        createTIAAFNALThomasCSV(account=account, debug=args.debug)
        createTIAAFNALThomasJSON(account=account, debug=args.debug)
    if args.csv:
        createTIAAFNALThomasCSV(account=account, debug=args.debug)
    if args.json:
        createTIAAFNALThomasJSON(account=account, debug=args.debug)
    if args.show:
        showTIAAFNALThomasRecords(account=account, debug=args.debug)