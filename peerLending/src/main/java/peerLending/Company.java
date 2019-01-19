package peerLending;


import java.util.*;


public class Company {
    private String username;
    private Map<Integer, Auction> auctionHistory; // auctions that are over
    private Map<Integer, Emission> emissionHistory; // emissions that are over


    // TODO: Pensar no caso em que o cliente vai abaixo antes das notificações serem guardadas?

    public Company(String username) {
        this.username = username;
        this.auctionHistory = new HashMap<Integer, Auction>();
        this.emissionHistory = new HashMap<Integer, Emission>();
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
