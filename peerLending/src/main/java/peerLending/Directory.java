package peerLending;

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

    public void putAuctionHistory(Auction auction, String company){
            // Remove auction from availableAuctions
            this.availableAuctions.remove(company);
            // Add auction to the history of the company
            this.companies.get(company).putAuction(auction);
    }

    public void putEmissionHistory(Emission emission, String company){
            // Remove emission from availableEmissions
            this.availableEmissions.remove(company);
            // Add emission to the history of the company
            this.companies.get(company).putEmission(emission);
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
