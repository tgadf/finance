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
        output['date']   = getDate(row['Date'])
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
            output['type']="RuthBankAccountWithdrawal"
        elif flag == "$":
            output['type']="RuthBankAccountCashWithdrawal"
        elif flag == "D":
            output['type']="RuthBankAccountDeposit"
        elif flag == "P":
            output['type']="RuthBankAccountPayment"
        elif flag == "C":
            output['type']="RuthBankAccountCheck"
        elif flag == "F":
            output['type']="RuthBankAccountFee"
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
    if payee == "ChaseBankAccount":
        category = account+"Receipt"
    output['category'] = category


    if output['category'] == account+"Receipt":
        output['transfer'] = "INTO"
    else:
        output['transfer'] = "NONE"


    # Check category
    checkCategory(category, row, payee)


    # Type
    if output['type'] == "RuthBankAccountDeposit":
        output['type'] = "Deposit"
    if output['type'] == "RuthBankAccountFee":
        output['type'] = "Charges"
    if output['type'] == "RuthBankAccountCheck":
        output['type'] = "Check"
    if output['type'] == "RuthBankAccountCashWithdrawal":
        output['type'] = "Cash"
    if output['type'] == "RuthBankAccountWithdrawal":
        output['type'] = "Withdrawal"
        

    # Check for None
    for k,v in output.iteritems():
        if v == None:
            print "row ->",row
            print "out ->",output
            raise ValueError("No",k," in row!")

    return output


def createRuthBankAccountCSV(basedir = '/Users/tgadfort/Documents/finance',
                   account = "RuthBankAccount", debug = False):
    fileio.removeSubPattern(basedir, [account, "csv"], ".csv", debug)
    files = fileio.findSubExt(basedir, [account, "original"], ".csv")
    for ifile in files:
        csvdata = fileio.get(ifile)
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



def createRuthBankAccountJSON(basedir = '/Users/tgadfort/Documents/finance',
                    account = "RuthBankAccount", debug = False):
    createJSON(account, basedir=basedir, debug=debug)
    showSummary(account, basedir=basedir, debug=debug, showYear = False)

def showRuthBankAccountRecords(basedir = '/Users/tgadfort/Documents/finance',
                     account = "RuthBankAccount", debug = False):
    showSummary(account, basedir=basedir, debug=debug, showYear = False)
    
def run(debug = False):
    createRuthBankAccountCSV(debug=debug)
    createRuthBankAccountJSON(debug=debug)


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

    account="RuthBankAccount"
    if args.run:
        createRuthBankAccountCSV(account=account, debug=args.debug)
        createRuthBankAccountJSON(account=account, debug=args.debug)
    if args.csv:
        createRuthBankAccountCSV(account=account, debug=args.debug)
    if args.json:
        createRuthBankAccountJSON(account=account, debug=args.debug)
    if args.show:
        showRuthBankAccountRecords(account=account, debug=args.debug)