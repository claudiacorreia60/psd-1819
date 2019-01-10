package peerLending;

import java.util.HashMap;
import java.util.Map;

public class Directory {
    private Map<String, Company> companies;
    private Map<String, Investor> investors;
    private Map<String, Auction> availableAuctions;
    private Map<String, Emission> availableEmissions;
    private int auctionCounter;
    private int emissionCounter;

    public Directory() {
        this.companies = new HashMap<String, Company>();
        this.investors = new HashMap<String, Investor>();
        this.availableAuctions = new HashMap<String, Auction>();
        this.availableEmissions = new HashMap<String, Emission>();
        this.auctionCounter = 0;
        this.emissionCounter = 0;
    }

    public String getClientPassword(String username){
        Company company = this.companies.get(username);

        if(company == null){
            Investor investor = this.investors.get(username);

            if(investor == null){
                // The client doesn't exist
                return null;
            }

            // The client is an investor
            return investor.getPassword();
        }

        // The client is a company
        return company.getPassword();
    }

    public int putBid(String company, String investor, int amount, float interest){
        Auction auction = this.availableAuctions.get(company);

        // Check if auction is available
        if(auction != null){
            // Insert bid
            Bid bid = new Bid(investor, amount, interest);
            auction.putBid(investor, bid);
            return 0;
        }

        // There's no available auction for company
        return -1;
    }

    public int putSubscription(String company, String investor, int amount){
        Emission emission = this.availableEmissions.get(company);

        // Check if emission is available
        if(emission != null){
            // Insert subscription
            emission.putSubscription(investor, amount);
            return 0;
        }

        // There's no available emission for company
        return -1;
    }

    public int putAuction(String company, int amount, float interest){
        // Create auction
        Auction auction = new Auction(this.auctionCounter, amount, interest);

        // Check if company has already an available auction
        if(this.availableAuctions.containsKey(company)){
            return -1;
        }

        // Add available auction
        this.availableAuctions.put(company, auction);

        // Increment auctionCounter
        this.auctionCounter++;

        return 0;
    }

    public int putEmission(String company, int amount, float interest){
        // Create emission
        Emission emission = new Emission(this.emissionCounter, amount, interest);

        // Check if company has already an available emission
        if(this.availableEmissions.containsKey(company)){
            return -1;
        }

        // Add available emission
        this.availableEmissions.put(company, emission);

        // Increment emissionCounter
        this.emissionCounter++;

        return 0;
    }

    public int endAuction(String company, Map<String, Bid> bids){
        Auction auction = this.availableAuctions.get(company);

        // Check if auction is available
        if(auction != null){
            auction.setBids(bids);
            // Remove auction from availableAuctions
            this.availableAuctions.remove(company);
            // Add auction to the history of the company
            this.companies.get(company).putAuction(auction);
            return 0;
        }

        return -1;
    }

    public int endEmission(String company, Map<String, Integer> subscriptions){
        Emission emission = this.availableEmissions.get(company);

        // Check if emission is available
        if(emission != null){
            emission.setSubscriptions(subscriptions);
            // Remove emission from availableEmissions
            this.availableEmissions.remove(company);
            // Add emission to the history of the company
            this.companies.get(company).putEmission(emission);
            return 0;
        }

        return -1;
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

    public Map<String, Company> getCompanies() {
        return companies;
    }
}
