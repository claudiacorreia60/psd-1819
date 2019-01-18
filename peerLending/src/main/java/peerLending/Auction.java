package peerLending;

import java.util.HashMap;
import java.util.Map;

public class Auction {
    private int id;
    private int amount;
    private float interest;
    private Map<String, Bid> bids;

    public Auction(int id, int amount, float interest) {
        this.id = id;
        this.amount = amount;
        this.interest = interest;
        this.bids = new HashMap<String, Bid>();
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

    @Override
    public String toString() {
        return "Auction{" +
                "id=" + id +
                ", amount=" + amount +
                ", interest=" + interest +
                ", bids=" + bids.toString() +
                '}';
    }
}
