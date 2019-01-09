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
        this.companies = new HashMap<>();
        this.investors = new HashMap<>();
        this.availableAuctions = new HashMap<>();
        this.availableEmissions = new HashMap<>();
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

    public void putBid(String company, String investor, int amount, float interest){
        Auction auction = this.availableAuctions.get(company);

        // Check if auction is available
        if(auction != null){
            // Insert bid
            Bid bid = new Bid(investor, amount, interest);
            auction.putBid(investor, bid);
        }
    }

    public void putSubscription(String company, String investor, int amount){
        Emission emission = this.availableEmissions.get(company);

        // Check if emission is available
        if(emission != null){
            // Insert subscription
            emission.putSubscription(investor, amount);
        }
    }

    public void putAuction(String company, int amount, float interest){
        // Create auction
        Auction auction = new Auction(this.auctionCounter, amount, interest);

        // Add available auction
        this.availableAuctions.put(company, auction);

        // Increment auctionCounter
        this.auctionCounter++;
    }

    public void putEmission(String company, int amount, float interest){
        // Create emission
        Emission emission = new Emission(this.emissionCounter, amount, interest);

        // Add available emission
        this.availableEmissions.put(company, emission);

        // Increment emissionCounter
        this.emissionCounter++;
    }
}
