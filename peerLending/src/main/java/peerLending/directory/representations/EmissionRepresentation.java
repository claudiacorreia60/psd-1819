package peerLending.directory.representations;

import com.fasterxml.jackson.annotation.*;
import peerLending.Emission;

import java.util.Map;

public class EmissionRepresentation {
    private int id;
    private int amount;
    private float interest;
    private Map<String, Integer> subscriptions;
    private String company;

    @JsonCreator
    public EmissionRepresentation(@JsonProperty("id") int id, @JsonProperty("amount") int amount, @JsonProperty("interest") float interest, @JsonProperty("subscriptions")  Map<String, Integer> subscriptions, @JsonProperty("company")  String company) {
        this.id = id;
        this.amount = amount;
        this.interest = interest;
        this.subscriptions = subscriptions;
        this.company = company;
    }

    public Emission build(){
        return new Emission(this.id, this.amount, this.interest, this.subscriptions);
    }

    public String getCompany() {
        return company;
    }
}
