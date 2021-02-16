package peerLending.directory.representations;

import com.fasterxml.jackson.annotation.*;
import peerLending.Emission;

import java.util.Map;

public class EmissionRepresentation {
    public int id;
    public String company;
    public int amount;
    public float interest;
    public Map<String, Integer> subscriptions;

    @JsonCreator
    public EmissionRepresentation(@JsonProperty("id") int id, @JsonProperty("company")  String company, @JsonProperty("amount") int amount, @JsonProperty("interest") float interest, @JsonProperty("subscriptions")  Map<String, Integer> subscriptions) {
        this.id = id;
        this.company = company;
        this.amount = amount;
        this.interest = interest;
        this.subscriptions = subscriptions;
        this.company = company;
    }

    public Emission build(){
        return new Emission(this.id, this.company, this.amount, this.interest, this.subscriptions);
    }
}
