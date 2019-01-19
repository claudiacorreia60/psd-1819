package peerLending;

import java.util.HashMap;
import java.util.Map;

public class Emission {
    private int id;
    private String company;
    private int amount;
    private float interest;
    private Map<String, Integer> subscriptions;

    public Emission(int id, String company, int amount, float interest) {
        this.id = id;
        this.company = company;
        this.amount = amount;
        this.interest = interest;
        this.subscriptions = new HashMap<String, Integer>();
    }

    public Emission(int id, String company, int amount, float interest, Map<String, Integer> subscriptions) {
        this.id = id;
        this.company = company;
        this.amount = amount;
        this.interest = interest;
        this.subscriptions = subscriptions;
    }

    public void putSubscription(String investor, int amount){
        this.subscriptions.put(investor, amount);
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

    public void setAmount(int amout) {
        this.amount = amout;
    }

    public float getInterest() {
        return interest;
    }

    public void setInterest(float interest) {
        this.interest = interest;
    }

    public Map<String, Integer> getSubscriptions() {
        return subscriptions;
    }

    public void setSubscriptions(Map<String, Integer> subscriptions) {
        this.subscriptions = subscriptions;
    }
}
