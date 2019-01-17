package peerLending;

import java.util.HashMap;
import java.util.Map;

public class Emission {
    private int id;
    private int amount;
    private int interest;
    private Map<String, Integer> subscriptions;

    public Emission(int id, int amount, int interest) {
        this.id = id;
        this.amount = amount;
        this.interest = interest;
        this.subscriptions = new HashMap<String, Integer>();
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

    public int getAmount() {
        return amount;
    }

    public void setAmount(int amout) {
        this.amount = amout;
    }

    public int getInterest() {
        return interest;
    }

    public void setInterest(int interest) {
        this.interest = interest;
    }

    public Map<String, Integer> getSubscriptions() {
        return subscriptions;
    }

    public void setSubscriptions(Map<String, Integer> subscriptions) {
        this.subscriptions = subscriptions;
    }

    @Override
    public String toString() {
        return "Emission{" +
                "id=" + id +
                ", amount=" + amount +
                ", interest=" + interest +
                ", subscriptions=" + subscriptions.toString() +
                '}';
    }
}
