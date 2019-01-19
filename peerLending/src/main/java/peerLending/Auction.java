package peerLending;

import java.util.HashMap;
import java.util.Map;

public class Auction {
    private int id;
    private String company;
    private int amount;
    private float interest;
    private Map<String, Bid> bids;

    public Auction(int id, String company, int amount, float interest) {
        this.id = id;
        this.company = company;
        this.amount = amount;
        this.interest = interest;
        this.bids = new HashMap<String, Bid>();
    }

    public Auction(int id, String company, int amount, float interest, Map<String, Bid> bids) {
        this.id = id;
        this.company = company;
        this.amount = amount;
        this.interest = interest;
        this.bids = bids;
    }

    public void putBid(String investor, Bid bid){
        this.bids.put(investor, bid);
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getCompany() {
        return company;
    }

    public void setCompany(String company) {
        this.company = company;
    }

    public int getAmount() {
        return amount;
    }

    public void setAmount(int amount) {
        this.amount = amount;
    }

    public float getInterest() {
        return interest;
    }

    public void setInterest(float interest) {
        this.interest = interest;
    }

    public Map<String, Bid> getBids() {
        return bids;
    }

    public void setBids(Map<String, Bid> bids) {
        this.bids = bids;
    }
}
