package peerLending.directory.representations;

import com.fasterxml.jackson.annotation.*;
import peerLending.Bid;

import java.util.Map;

public class BidRepresentation {
    private String investor;
    private int amount;
    private float interest;

    @JsonCreator
    public BidRepresentation(@JsonProperty("investor") String investor, @JsonProperty("amount") int amount, @JsonProperty("interest") float interest) {
        this.investor = investor;
        this.amount = amount;
        this.interest = interest;
    }

    public Bid build(){
        return new Bid(this.investor, this.amount, this.interest);
    }
}
