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
from strdate import getDate, getYearMonth, getMDY
from convertBase import showSummary, createJSON, writeCSVFile, initCSVoutput 
from edits import applyEdits

    
##########################################
#
# Parse output from CSV to produce CSV
#
##########################################
def parseRow(row, account):
    output = initCSVoutput()
    output['account'] = account

    # Date
    if row.get('Date'):
        output['date']=getDate(row['Date'])
        mon,day,year     = getMDY(output['date'])
        output['month']  = int(mon)
        output['day']    = int(day)
        output['year']   = int(year)



    # Baby
    if row.get('Baby'):
        output['baby']=row['Baby']


    # Description
    if row.get('Description'):
        output['description']=row['Description']
        output["type"]=row['Description']

    # Amount
    if row.get('Amount'):
        output['amount']=float(row['Amount'])
        
    # Statement
    output['statement'] = getYearMonth(output['date'])
    
    # Original
    output['original'] = "Custom"

    # Payee
    output['payee']= account
    
    
    # Category
    if output["description"] == "Contribution":
        amt = int(output['amount'])
        if amt in [400]:
            output['category'] = account + "Receipt"
        else:
            output['category'] = account + "GiftReceipt"
        output["type"] = "Receipt"
    elif output["description"].find("Contribution AIP") != -1:
        output['category'] = account + "Receipt"
        output["type"] = "Receipt"
    elif output["description"].find("Contribution EBT") != -1:
        output['category'] = account + "Receipt"
        output["type"] = "Receipt"
    elif output["description"] == "Fee":
        output['category'] = account + "Charges"
        output["type"] = "Charges"
    elif output["description"] == "Earnings":
        output['category'] = account + "Interest"
        output["type"] = "Interest"
    elif output["description"].find("OpeningBalance") != -1:
        output['category'] = account + "OpeningBalance"
        output["type"] = "OpeningBalance"
    else:
        print output
        print "I don't recognize the description!"
        raise()


    # Payee and Transfer
    if output['category'] == account + "Receipt":
        output['transfer'] = "INTO"
        output['payee']    = "ChaseBankAccount"
    elif output['category'] == account + "GiftReceipt":
        output['transfer'] = "INTO"
        output['payee']    = "ChaseBankAccount"
    else:
        output['transfer'] = "NONE"
        output['payee']    = account


    # Check for None
    for k,v in output.iteritems():
        if v == None:
            print "No",k," in row!"
            print "row ->",row
            print "out ->",output
            raise()
    

    return output


def createClaire529NYCSV(basedir = '/Users/tgadfort/Documents/finance',
                   account = "Claire529NY", debug = False):
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



def createClaire529NYJSON(basedir = '/Users/tgadfort/Documents/finance',
                           account = "Claire529NY", debug = False):
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)

def showClaire529NYRecords(basedir = '/Users/tgadfort/Documents/finance',
                            account = "Claire529NY", debug = False):
    showSummary(account, basedir=basedir, debug=debug, showYear = False)
    
def run(debug = False):
    createClaire529NYCSV(debug=debug)
    createClaire529NYJSON(debug=debug)


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

    account="Claire529NY"
    if args.run:
        createClaire529NYCSV(account=account, debug=args.debug)
        createClaire529NYJSON(account=account, debug=args.debug)
    if args.csv:
        createClaire529NYCSV(account=account, debug=args.debug)
    if args.json:
        createClaire529NYJSON(account=account, debug=args.debug)
    if args.show:
        showClaire529NYRecords(account=account, debug=args.debug)