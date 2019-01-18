package peerLending.directory.representations;

import com.fasterxml.jackson.annotation.*;
import peerLending.Auction;
import peerLending.Bid;

import java.util.HashMap;
import java.util.Map;

public class AuctionRepresentation {
    private int id;
    private int amount;
    private float interest;
    private Map<String, BidRepresentation> bids;
    private String company;

    @JsonCreator
    public AuctionRepresentation(@JsonProperty("id") int id, @JsonProperty("amount") int amount, @JsonProperty("interest") float interest, @JsonProperty("bids")  Map<String, BidRepresentation> bids, @JsonProperty("company")  String company) {
        this.id = id;
        this.amount = amount;
        this.interest = interest;
        this.bids = bids;
        this.company = company;
    }

    public Auction build(){
        Map<String, Bid> bids = new HashMap<String, Bid>();

        for(Map.Entry<String, BidRepresentation> e : this.bids.entrySet()){
            bids.put(e.getKey(), e.getValue().build());
        }

        return new Auction(this.id, this.amount, this.interest, bids);
    }

    public String getCompany() {
        return company;
    }
}
