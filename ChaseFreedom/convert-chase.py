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

from fileio import get
from search import removeSubPattern, findSubExt
from strdate import getDate, getMDY
from cleanuppayee import cleanUpPayee
from convertBase import showSummary, createJSON, writeCSVFile, initCSVoutput
from edits import applyEdits, getCorrPayee, isKnownPayee, getPayeeCategory, checkCategory, checkPayee

    
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
        if flag == "W":
            output['type']="ChaseFreedomWithdrawal"
        elif flag == "$":
            output['type']="ChaseFreedomCashWithdrawal"
        elif flag == "D":
            output['type']="ChaseFreedomDeposit"
        elif flag == "P":
            output['type']="ChaseFreedomPayment"
        elif flag == "C":
            output['type']="ChaseFreedomCheck"
        elif flag == "F":
            output['type']="ChaseFreedomFee"
        else:
            print "Type:",flag,"not known."
            raise()


    # Payee
    if row.get("Payee"):
        output['original'] = row['Payee']
        retval,cleanpayee=cleanUpPayee(row['Payee'])
        payee = getCorrPayee(cleanpayee)

        if row.get("Known"):
            known=row['Known']
            if known != payee:
                #print '->',known
                if isKnownPayee(known):
                    payee=known
        #print '-->',payee
        if not isKnownPayee(payee):
            testpayee = getCorrPayee(row["Payee"])
            if isKnownPayee(testpayee):
                payee=testpayee
        #print '-->',payee

        checkPayee(payee, row)
        output['payee'] = payee


    # Category
    category = getPayeeCategory(payee)
    output['category'] = category


    if output['payee'] == "ChaseBankAccount":
        if output['amount'] > 0:
            output['transfer'] = "INTO"
            output['category'] = "ChaseFreedomReceipt"
        else:
            output['transfer'] = "OUT"
            output['category'] = "ChaseFreedomPayment"
    elif output['category'] == "CreditCardFreedomPayment":
        output['transfer'] = "OUT"
    elif output['category'] == "CreditCardPayment":
        output['transfer'] = "OUT"
    elif output['payee'] == "IRAPayment":
        output['transfer'] = "OUT"
    elif output['payee'] == "529Payment":
        output['transfer'] = "OUT"
    elif output['payee'] == "HSBCTransfer":
        if output['amount'] > 0:
            output['transfer'] = "INTO"
            output['category'] = "HSBCContributionReceipt"
        else:
            output['transfer'] = "OUT"
            output['category'] = "HSBCContributionPayment"
    else:
        output['transfer'] = "NONE"


    # Check category
    checkCategory(category, row, payee)


    # Type
    if output['type'] == "ChaseFreedomDeposit":
        output['type'] = "Deposit"
    if output['type'] == "ChaseFreedomFee":
        output['type'] = "Charges"
    if output['type'] == "ChaseFreedomCheck":
        output['type'] = "Check"
    if output['type'] == "ChaseFreedomCashWithdrawal":
        output['type'] = "Cash"
    if output['type'] == "ChaseFreedomWithdrawal":
        output['type'] = "Withdrawal"
        

    # Check for None
    for k,v in output.iteritems():
        if v == None:
            print "row ->",row
            print "out ->",output
            raise ValueError("No",k," in row!")

    return output


def createChaseFreedomCSV(basedir = '/Users/tgadfort/Documents/finance',
                   account = "ChaseFreedom", debug = False):
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
        data = applyEdits(data, debug=True)
        writeCSVFile(data, basedir=basedir, account=account, debug=debug)


def createChaseFreedomJSON(basedir = '/Users/tgadfort/Documents/finance',
                    account = "ChaseFreedom", debug = False):
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)

def showChaseFreedomRecords(basedir = '/Users/tgadfort/Documents/finance',
                     account = "ChaseFreedom", debug = False):
    showSummary(account, basedir=basedir, debug=debug, showYear = False)
    
def run(debug = False):
    createChaseFreedomCSV(debug=debug)
    createChaseFreedomJSON(debug=debug)


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

    account="ChaseFreedom"
    if args.run:
        createChaseFreedomCSV(account=account, debug=args.debug)
        createChaseFreedomJSON(account=account, debug=args.debug)
    if args.csv:
        createChaseFreedomCSV(account=account, debug=args.debug)
    if args.json:
        createChaseFreedomJSON(account=account, debug=args.debug)
    if args.show:
        showChaseFreedomRecords(account=account, debug=args.debug)