package peerLending.directory.representations;

import com.fasterxml.jackson.annotation.*;

import java.util.Map;

public class CompanyRepresentation {
    public String username;
    public Map<Integer, AuctionRepresentation> auctionHistory;
    public Map<Integer, EmissionRepresentation> emissionHistory;

    @JsonCreator
    public CompanyRepresentation(@JsonProperty("username") String username, @JsonProperty("auctionHistory") Map<Integer, AuctionRepresentation> auctionHistory, @JsonProperty("emissionHistory") Map<Integer, EmissionRepresentation> emissionHistory) {
        this.username = username;
        this.auctionHistory = auctionHistory;
        this.emissionHistory = emissionHistory;
    }
}
