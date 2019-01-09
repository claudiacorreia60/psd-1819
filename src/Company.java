import java.util.HashMap;
import java.util.Map;

public class Company {
    private String username;
    private String password;
    private Map<Integer, Auction> auctionHistory;
    private Map<Integer, Emission> emissionHistory;

    public Company(String username, String password) {
        this.username = username;
        this.password = password;
        this.auctionHistory = new HashMap<>();
        this.emissionHistory = new HashMap<>();
    }

    public void putAuction(Auction auction){
        this.auctionHistory.put(auction.getId(), auction);
    }

    public void putEmission(Emission emission){
        this.emissionHistory.put(emission.getId(), emission);
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public Map<Integer, Auction> getAuctionHistory() {
        return auctionHistory;
    }

    public void setAuctionHistory(Map<Integer, Auction> auctionHistory) {
        this.auctionHistory = auctionHistory;
    }

    public Map<Integer, Emission> getEmissionHistory() {
        return emissionHistory;
    }

    public void setEmissionHistory(Map<Integer, Emission> emissionHistory) {
        this.emissionHistory = emissionHistory;
    }
}
