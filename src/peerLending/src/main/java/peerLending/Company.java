package peerLending;


import java.util.*;


public class Company {
    private String username;
    private Map<Integer, Auction> auctionHistory; // auctions that are over
    private Map<Integer, Emission> emissionHistory; // emissions that are over

    public Company(String username) {
        this.username = username;
        this.auctionHistory = new HashMap<Integer, Auction>();
        this.emissionHistory = new HashMap<Integer, Emission>();
    }

    public Map<Integer, Auction> getSuccessfulAuctions(){
        Map<Integer, Auction> successfulAuctions = new HashMap<Integer, Auction>();

        // Get successful Auctions
        for(Map.Entry<Integer, Auction> e : this.auctionHistory.entrySet()){
            if(e.getValue().successful()){
                successfulAuctions.put(e.getKey(), e.getValue());
            }
        }

        return successfulAuctions;
    }

    public Auction getLastSuccessfulAuction() {
        Map<Integer, Auction> successfulAuctions = this.getSuccessfulAuctions();

        // Convert Map to List of Map
        List<Map.Entry<Integer, Auction>> list =
                new LinkedList<Map.Entry<Integer, Auction>>(successfulAuctions.entrySet());

        // Sort list with Collections.sort()
        Collections.sort(list, new Comparator<Map.Entry<Integer, Auction>>() {
            public int compare(Map.Entry<Integer, Auction> o1,
                               Map.Entry<Integer, Auction> o2) {
                return ((Integer) o2.getValue().getId()).compareTo(o1.getValue().getId());
            }
        });

        if(list.isEmpty()){
            return null;
        }

        return list.get(0).getValue();
    }

    public Emission getLastEmission() {

        // Convert Map to List of Map
        List<Map.Entry<Integer, Emission>> list =
                new LinkedList<Map.Entry<Integer, Emission>>(this.emissionHistory.entrySet());

        // Sort list with Collections.sort()
        Collections.sort(list, new Comparator<Map.Entry<Integer, Emission>>() {
            public int compare(Map.Entry<Integer, Emission> o1,
                               Map.Entry<Integer, Emission> o2) {
                return ((Integer) o2.getValue().getId()).compareTo(o1.getValue().getId());
            }
        });

        if(list.isEmpty()){
            return null;
        }

        return list.get(0).getValue();
    }

    public float getLowestEmissionRate() {
        Map<Integer, Emission> allEmissions = this.emissionHistory;
        Set<Float> interests = new HashSet<Float>();

        for(Emission emission : allEmissions.values()){
            if(emission.successful()){
                interests.add(emission.getInterest());
            }
        }

        return Collections.min(interests);
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
