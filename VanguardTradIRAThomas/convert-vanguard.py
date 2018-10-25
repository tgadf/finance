# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 10:39:07 2015

@author: tgadfort
"""

import sys
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')
    sys.path.insert(0, '/Users/tgadfort/Python')

from fileio import getCSV
from search import removeSubPattern, findSubExt
import argparse
from location import getBaseLocation
from strdate import getDate, getYearMonth, getMDY
from convertBase import showSummary, createJSON, writeCSVFile, initCSVoutput
from edits import applyEdits
from data import fix

    
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


    # Statement
    output['statement']=getYearMonth(output['date'])


    # Original
    output['original']=row['original']


    # Category
    if output['type'] == "Opening Balance":
        output['category'] = account+"OpeningBalance"
        output['payee'] = account
    elif output['type'] == "Interest":
        output['category'] = account+"Interest"
        output['payee'] = account
    elif output['type'] == "Contribution":
        output['category'] = account+"ContributionReceipt"
        output['payee'] = "ChaseBankAccount"
    else:
        raise ValueError("Did not recognize type:",output['type'])


    # Type
    output['type'] = output['category'].replace(account,"")
    output['type'] = output['type'].replace("ContributionReceipt","Contribution")

    # Transfer
    if output['category'] == account+"ContributionReceipt":
        output['transfer']="INTO"
    else:
        output['transfer']="NONE"

          

    # Check for None
    for k,v in output.iteritems():
        if v == None:
            print "No",k,"in row!"
            print "  row  ->",row
            print "  out  ->",output
            print "  fund ->",output['fund']
            raise()

    return output


def createVanguardTradIRAThomasBalance(account = "VanguardTradIRAThomas", debug = False):
    basedir = getBaseLocation()
    removeSubPattern(basedir, [account, "original"], ".csv", debug)
    fix.createOpening()
    fix.createActivity()
    fix.parsePDFs()
    fix.makeCSVs() # This write the original file



def createVanguardTradIRAThomasCSV(account = "VanguardTradIRAThomas", debug = False):
    basedir = getBaseLocation()
    removeSubPattern(basedir, [account, "csv"], ".csv", debug)
    files = findSubExt(basedir, [account, "original"], ".csv")
    for ifile in files:
        csvdata  = getCSV(ifile, delimiter=",")
        data=[]
        nrows=0
        for row in csvdata:
            nrows += 1
            vals=parseRow(row, account)
            if vals:
                data.append(vals)
        print "\tProcessed",len(data),"/",nrows,"rows in",ifile
        data = applyEdits(data, debug=debug)
        writeCSVFile(data, basedir=basedir, account=account, debug=debug)



def createVanguardTradIRAThomasJSON(account = "VanguardTradIRAThomas", debug = False):
    basedir = getBaseLocation()
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)



def showVanguardTradIRAThomasRecords(account = "VanguardTradIRAThomas", debug = False):
    basedir = getBaseLocation()
    showSummary(account, basedir=basedir, debug=debug, showYear = False)
    
    
    
def run(debug = False):
    createVanguardTradIRAThomasBalance(debug=debug)
    createVanguardTradIRAThomasCSV(debug=debug)
    createVanguardTradIRAThomasJSON(debug=debug)


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

    account="VanguardTradIRAThomas"
    if args.run:
        createVanguardTradIRAThomasCSV(account=account, debug=args.debug)
        createVanguardTradIRAThomasJSON(account=account, debug=args.debug)
    if args.csv:
        createVanguardTradIRAThomasCSV(account=account, debug=args.debug)
    if args.json:
        createVanguardTradIRAThomasJSON(account=account, debug=args.debug)
    if args.show:
        showVanguardTradIRAThomasRecords(account=account, debug=args.debug)