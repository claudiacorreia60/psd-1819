package peerLending.directory;

import peerLending.Auction;
import peerLending.Company;
import peerLending.Emission;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class Directory {
    private Map<String, Company> companies;
    private Map<String, Auction> availableAuctions;
    private Map<String, Emission> availableEmissions;

    public Directory() {
        this.companies = new HashMap<String, Company>();
        this.availableAuctions = new HashMap<String, Auction>();
        this.availableEmissions = new HashMap<String, Emission>();
    }

    public void putAvailableAuction(Auction auction){
        // Add available auction
        this.availableAuctions.put(auction.getCompany(), auction);
    }

    public void putAvailableEmission(Emission emission){
        // Add available emission
        this.availableEmissions.put(emission.getCompany(), emission);
    }

    public int putAuctionHistory(Auction auction){
        // Remove auction from availableAuctions
        this.availableAuctions.remove(auction.getCompany());

        Company c = this.companies.get(auction.getCompany());

        // Company does not exist
        if(c == null){
            return -1;
        }

        // Add auction to the history of the company
        c.putAuction(auction);
        return 0;
    }

    public int putEmissionHistory(Emission emission){
        // Remove emission from availableEmissions
        this.availableEmissions.remove(emission.getCompany());

        Company c = this.companies.get(emission.getCompany());

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

    public Set<String> getCompaniesNames(){
        return this.companies.keySet();
    }

    public Company getCompanyHistory(String company){
        return this.companies.get(company);
    }

    public Map<String, Auction> getAvailableAuctions(){
        return this.availableAuctions;
    }

    public Map<String, Emission> getAvailableEmissions(){
        return this.availableEmissions;
    }
}
