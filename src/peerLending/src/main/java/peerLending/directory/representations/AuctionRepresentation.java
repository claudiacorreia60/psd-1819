package peerLending.directory.representations;

import com.fasterxml.jackson.annotation.*;
import peerLending.Auction;
import peerLending.Bid;

import java.util.HashMap;
import java.util.Map;

public class AuctionRepresentation {
    public int id;
    public String company;
    public int amount;
    public float interest;
    public Map<String, BidRepresentation> bids;

    @JsonCreator
    public AuctionRepresentation(@JsonProperty("id") int id, @JsonProperty("company")  String company, @JsonProperty("amount") int amount, @JsonProperty("interest") float interest, @JsonProperty("bids")  Map<String, BidRepresentation> bids) {
        this.id = id;
        this.company = company;
        this.amount = amount;
        this.interest = interest;
        this.bids = bids;
    }

    public Auction build(){
        Map<String, Bid> bids = new HashMap<String, Bid>();

        if(this.bids != null) {
            for (Map.Entry<String, BidRepresentation> e : this.bids.entrySet()) {
                bids.put(e.getKey(), e.getValue().build());
            }

            return new Auction(this.id, this.company, this.amount, this.interest, bids);
        }

        return new Auction(this.id, this.company, this.amount, this.interest, null);
    }
}
