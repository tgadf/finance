#include "TDatime.h"
#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <vector>
#include <map>
#include <TStopwatch.h>
#include <TFile.h>
#include <TTree.h>

using namespace std;

map<int, int> in_out;
map<string, int> categories;
map<int, string> categories_id;

map<string, int> payee_id;
map<int, string> id_payee;

#include "Categories/Categories.C"
#include "Payees/PayeeIDMap.C"


void MakeMoneyTree()
{
  LoadCategories(&categories, &categories_id, &in_out);

  PayeeIDMap(&payee_id, &id_payee);

  TStopwatch w;
  w.Start();

  ifstream in;
  in.open("Formatted/mymoney2.dat");
  if ( !in.is_open() ) { cout << "Could not find Formatted/mymoney2.dat" << endl; return; }

  int entry = 0;
  string uid;
  string date;
  double amount;
  string category;
  string account;
  string payee;
  string dummy;

  Int_t date_t;
  Double_t amount_t;
  Int_t category_t;
  Int_t payee_t;
  Int_t transfer_out_t;
  Int_t transfer_in_t;
  Int_t credit_t;
  Int_t saving_t;
  
  TDatime time;

  map<string, int> transfer_in;

  TFile *file = new TFile("money.root", "RECREATE");
  TTree *tree = new TTree("money", "");
  tree->Branch("date", &date_t, "date/I");
  tree->Branch("amount", &amount_t, "amount/D");
  tree->Branch("payee", &payee_t, "payee/I");
  tree->Branch("category", &category_t, "category/I");
  tree->Branch("transfer_in", &transfer_in_t, "transfer_in/I");
  tree->Branch("transfer_out", &transfer_out_t, "transfer_out/I");
  tree->Branch("saving", &saving_t, "saving/I");
  tree->Branch("credit", &credit_t, "credit/I");

  int i = 0;
  while ( in.eof() == false ) {
    i++;
    entry = i;
    in >> uid >> dummy >> date >> dummy >> amount >> dummy >> payee >> dummy >> category >> dummy >> account;
    if ( date == "---" ) { 
      cout << "Stopping at " << entry-1 << endl;
      break;
    }

    if ( 0 ) {
    if ( payee.find("TRANSFER") != string::npos ) {
      if ( payee.find(account) == string::npos ) {
	cout << entry << "\t" << date << "\t" << amount << "\t" << payee << "\t" << category << "\t" << account << endl;
      }
      else {
	cout << entry << "\t" << date << "\t" << amount << "\t" << payee << "\t" << category << "\t" << account << endl;
	cout << "Transfer is weird!!!" << endl;
	return;
      }
    }
    }
    
    // Special Stuff for transfers
    transfer_in_t = 0;
    transfer_out_t = 0;
    saving_t = 0;
    credit_t = 0;
    if ( category == "CollegeFund" ) { saving_t = 1; }
    if ( category == "ThomasIRA" ) { saving_t = 1; }
    if ( category == "RuthIRA" ) { saving_t = 1; }
    if ( category == "HomeDownPayment" ) {
      // special $90,000 transfer
      if ( account == "HSBC" && amount == -90000 ) { transfer_in_t = 1; saving_t = 1; }
      if ( account == "Chase" && amount == 90000 ) { transfer_in_t = 1; saving_t = 0; }
      //cout << payee << "\t" << account << "\t" << date << "\t" << category << "\t" << amount << endl;
    }
    if ( category == "HouseDownPayment" ) {
      if ( account == "Chase" ) {
	if ( payee == "HSBCTransfer" || payee == "HSBCDirect" ) { transfer_in_t = 1; }
	else {
	  cout << payee << "\t" << date << "\t" << category << "\t" << amount << endl;
	}
      }
      if ( account == "HSBC" ) {
	if ( amount > 0 ) { transfer_out_t = 1; saving_t = 1; }
      }
    }
    if ( category == "CreditCard" ) { credit_t = 1; }

    if ( 0 ) {
      if ( saving_t == 1 ) {
	cout << "Savings ---> " << entry << "\t" << date << "\t" << amount << "\t" << payee << "\t" << category << "\t" << account << endl;
      }
      if ( credit_t == 1 ) {
	cout << "Credit --->  " << entry << "\t" << date << "\t" << amount << "\t" << payee << "\t" << category << "\t" << account << endl;
      }
      if ( transfer_in_t == 1 ) {
	cout << "IN ---> " << entry << "\t" << date << "\t" << amount << "\t" << payee << "\t" << category << "\t" << account << endl;
      }
      if ( transfer_out_t == 1 ) {
	cout << "OUT --> " << entry << "\t" << date << "\t" << amount << "\t" << payee << "\t" << category << "\t" << account << endl;
      }
    }

      
      
    if ( entry % 100 == 0 && entry > 0 && 0 ) {
      w.Stop();
      double mytime = w.RealTime();
      cout.precision(3);
      cout << "Entry " << entry << " : Time " << mytime << " : Rate " << entry / mytime << endl;
      w.Continue();
    }


    string comp = "/";
    size_t previous_found = 0;
    int nfound = 0;
    size_t found = date.find(comp);
    string month, day, year;
    int month_i, day_i, year_i;
    stringstream month_ss, day_ss, year_ss, time_ss;
    

    while ( found != string::npos ) {
      string value = date.substr(previous_found, found-previous_found);
      if ( nfound == 0 ) { month = value; }
      if ( nfound == 1 ) { day = value; }
      int tmp_found = found;
      found = date.find(comp,found+1);
      previous_found = tmp_found+1;
      nfound++;
      if ( found == string::npos ) {
	year = date.substr(previous_found, found-previous_found);
	year = year;
	break;
      }
    }
    
    month_ss << month;
    day_ss << day;
    year_ss << year;
    month_ss >> month_i;
    day_ss >> day_i;
    year_ss >> year_i;
    //cout << month << " , " << day << " , " << year << endl;
    if ( month_i < 10 ) {
      if ( day_i < 10 ) {
	time_ss << "20" << year << "-" << "0" << month << "-" << "0" << day << " 01:01:01";
      }
      else {
	time_ss << "20" << year << "-" << "0" << month << "-" << "" << day << " 01:01:01";
      }
    }
    else {
      if ( day_i < 10 ) {
	time_ss << "20" << year << "-" << "" << month << "-" << "0" << day << " 01:01:01";
      }
      else {
	time_ss << "20" << year << "-" << "" << month << "-" << "" << day << " 01:01:01";
      }
    }

    if ( year_i < 100 ) { year_i += 2000; }
    
    //cout << i << "\t" << year_i << "\t" << date << "\t" << time_ss.str() << endl;
    int time_i = day_i + 100*month_i + 10000*year_i;
    //time_ss >> time_i;
    //cout << time_i << "\t";
    time.Set(time_i, 1);
    //cout << time.Get() << "\t";
    //time.Set(year_i, month_i, day_i, 0, 0, 0);
    date_t = time.GetDate();
    //cout << date_t << "\t" << time_ss.str() << "\t";
    //cout << time.GetYear() << "/" << time.GetMonth() << "/" << time.GetDay() << endl;
    amount_t = amount;
    category_t = categories[category];
    payee_t = payee_id[payee];

    if ( payee_t < 1 ) {
      cout << "Payee_t == 0: " << payee_t << "\t" << payee << endl;
    }

    //if ( account == "HSBC" ) {
    //category_t = categories["HSBCSavingsAccount"];
    //}
    if ( category_t < 1 ) {
      cout << "Category_t == 0: " << category_t << "\t" << category << endl;
    }
    tree->Fill();
  }

  cout << "Wrote " << tree->GetEntries() << " entries into money tree." << endl;
  cout << "Writing..." << endl;
  file->Write();
  cout << "Closing..." << endl;
  file->Close();
  cout << "Done." << endl;
  cout << endl;
}
