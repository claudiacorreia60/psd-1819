public class Bid {
    private String investor;
    private int amount;
    private float interest;

    public Bid(String investor, int amount, float interest) {
        this.investor = investor;
        this.amount = amount;
        this.interest = interest;
    }

    public String getInvestor() {
        return investor;
    }

    public void setInvestor(String investor) {
        this.investor = investor;
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

    public void setInterest(int interest) {
        this.interest = interest;
    }
}
