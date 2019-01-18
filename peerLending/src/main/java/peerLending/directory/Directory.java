package peerLending.directory;

import peerLending.Auction;
import peerLending.Company;
import peerLending.Emission;

import java.util.HashMap;
import java.util.Map;

public class Directory {
    private Map<String, Company> companies;
    private Map<String, Auction> availableAuctions;
    private Map<String, Emission> availableEmissions;

    public Directory() {
        this.companies = new HashMap<String, Company>();
        this.availableAuctions = new HashMap<String, Auction>();
        this.availableEmissions = new HashMap<String, Emission>();
    }

    public void putAvailableAuction(Auction auction, String company){
        // Add available auction
        this.availableAuctions.put(company, auction);
    }

    public void putAvailableEmission(Emission emission, String company){
        // Add available emission
        this.availableEmissions.put(company, emission);
    }

    public int putAuctionHistory(Auction auction, String company){
        // Remove auction from availableAuctions
        this.availableAuctions.remove(company);

        Company c = this.companies.get(company);

        // Company does not exist
        if(c == null){
            return -1;
        }

        // Add auction to the history of the company
        c.putAuction(auction);
        return 0;
    }

    public int putEmissionHistory(Emission emission, String company){
        // Remove emission from availableEmissions
        this.availableEmissions.remove(company);

        Company c = this.companies.get(company);

        // Company does not exist
        if(c == null){
            return -1;
        }

        // Add emission to the history of the company
        c.putEmission(emission);
        return 0;
    }

    public void putCompany(Company company){
        this.companies.put(company.getUsername(), company);
    }

    public String getCompaniesNames(){
        return this.companies.keySet().toString();
    }

    public String getCompanyHistory(String company){
        return this.companies.get(company).toString();
    }

    public String getAvailableAuctions(){
        return this.availableAuctions.toString();
    }

    public String getAvailableEmissions(){
        return this.availableEmissions.toString();
    }
}
