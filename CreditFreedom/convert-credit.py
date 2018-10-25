# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 10:39:07 2015

@author: tgadfort
"""

import sys
import argparse
if '/Users/tgadfort/Python' not in sys.path:
    sys.path.insert(0, '/Users/tgadfort/Documents/finance/Perm')
    sys.path.insert(0, '/Users/tgadfort/Python')

from location import getBaseLocation
from fileio import get
from search import findSubExt, removeSubPattern
from strdate import getDate, getMDY
from cleanuppayee import cleanUpPayee
from convertBase import showSummary, createJSON, writeCSVFile, initCSVoutput
from edits import applyEdits, getCorrPayee, isKnownPayee, getPayeeCategory, checkCategory, checkPayee
from data import fix

    
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

        
        
    # Description
    if row.get('Statement'):
        output['statement']=row['Statement']



    # Amount
    if row.get('Amount'):
        output['amount']=float(row['Amount'])



    # Type
    if row.get('Type'):
        flag=row['Type']
        if flag == "Pur":
            output['type']="CreditPurchase"
        elif flag == "$":
            output['type']="CreditCashAdvance"
        elif flag == "Pay":
            output['type']="CreditPayment"
        elif flag == "C":
            output['type']="CreditRedemption"
        elif flag == "F":
            output['type']="CreditFee"
        elif flag == "D":
            output['type']="OpeningBalance"
        else:
            print "Type:",flag,"not known."
            raise()

    # Payee
    if row.get("Payee"):
        output['original'] = row['Payee']
        retval,cleanpayee=cleanUpPayee(row['Payee'])
        payee=getCorrPayee(cleanpayee)

        if row.get("Known"):
            known=row['Known']
            if known != payee:
                if isKnownPayee(known):
                    payee=known
                    
                    
        checkPayee(payee, row)
        output['payee'] = payee



    # Category
    category = getPayeeCategory(payee)
    output['category'] = category
    checkCategory(category, row, payee)



    # Transfer
    output['transfer'] = None
    if output['category'] == account+"Receipt":
        output['transfer'] = "INTO"
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



def createCreditFreedomOriginal(account = "CreditFreedom", debug = False):
    basedir = getBaseLocation()
    removeSubPattern(basedir, [account, "original"], ".csv", debug)
    fix.createOpening()
    fix.createActivity()
    fix.parsePDFs()
    fix.makeCSVs() # This write the original file



def createCreditFreedomCSV(account = "CreditFreedom", debug = False):
    basedir = getBaseLocation()
    removeSubPattern(basedir, [account, "csv"], ".csv", debug)    
    files = findSubExt(basedir, [account, "original"], ".csv")
    for ifile in files:
        csvdata = get(ifile)
        data=[]
        nrows=0
        for row in csvdata:
            nrows += 1
            if row['Entry'].count("#") > 0:
                continue
            vals=parseRow(row, account)
            data.append(vals)
        print "\tProcessed",len(data),"/",nrows,"rows in",ifile
        data = applyEdits(data, debug=debug)
        writeCSVFile(data, basedir=basedir, account=account, debug=debug)



def createCreditFreedomJSON(account = "CreditFreedom", debug = False):
    basedir = getBaseLocation()
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)



def showCreditFreedomRecords(account = "CreditFreedom", debug = False):
    basedir = getBaseLocation()
    showSummary(account, basedir=basedir, debug=debug, showYear = False)


    
def run(debug = False):
    createCreditFreedomCSV(debug=debug)
    createCreditFreedomJSON(debug=debug)


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

    account="Credit"
    if args.run:
        createCreditCSV(account=account, debug=args.debug)
        createCreditJSON(account=account, debug=args.debug)
    if args.csv:
        createCreditCSV(account=account, debug=args.debug)
    if args.json:
        createCreditJSON(account=account, debug=args.debug)
    if args.show:
        showCreditRecords(account=account, debug=args.debug)