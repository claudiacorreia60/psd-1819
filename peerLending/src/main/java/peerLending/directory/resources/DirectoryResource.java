package peerLending.directory.resources;

import javax.ws.rs.core.Response;
import peerLending.Auction;
import peerLending.Bid;
import peerLending.Company;
import peerLending.Emission;
import peerLending.directory.Directory;
import peerLending.directory.representations.AuctionRepresentation;
import peerLending.directory.representations.BidRepresentation;
import peerLending.directory.representations.CompanyRepresentation;
import peerLending.directory.representations.EmissionRepresentation;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

@Path("/directory")
@Produces(MediaType.APPLICATION_JSON)
public class DirectoryResource {
    private Directory directory;

    public DirectoryResource() {
        this.directory = new Directory();
        Company c1 = new Company("apple");
        this.directory.putCompany(c1);
        Company c2 = new Company("ibm");
        this.directory.putCompany(c2);
        Company c3 = new Company("google");
        this.directory.putCompany(c3);
        Company c4 = new Company("primavera");
        this.directory.putCompany(c4);
        Company c5 = new Company("edp");
        this.directory.putCompany(c5);
        Company c6 = new Company("farfetch");
        this.directory.putCompany(c6);
    }

    // Consult companies
    @GET
    @Path("/companies")
    public Set<String> getCompanies() {
        return this.directory.getCompaniesNames();
    }

    // Consult company history
    @GET
    @Path("/company/{name}")
    public Response getCompanyHistory(@PathParam("name") String name) {
        Map<Integer, AuctionRepresentation> auctionRes = new HashMap<Integer, AuctionRepresentation>();
        Map<Integer, EmissionRepresentation> emissionRes = new HashMap<Integer, EmissionRepresentation>();
        Company company = this.directory.getCompanyHistory(name);

        for(Map.Entry<Integer, Auction> e : company.getAuctionHistory().entrySet()){
            Auction auction = e.getValue();
            Map<String, BidRepresentation> bidsRes = new HashMap<String, BidRepresentation>();

            if(auction.getBids() != null) {
                for (Map.Entry<String, Bid> eBid : auction.getBids().entrySet()) {
                    Bid bid = eBid.getValue();
                    bidsRes.put(eBid.getKey(), new BidRepresentation(bid.getInvestor(), bid.getAmount(), bid.getInterest()));
                }

                auctionRes.put(e.getKey(), new AuctionRepresentation(auction.getId(), auction.getCompany(), auction.getAmount(), auction.getInterest(), bidsRes));
            }

            else {
                auctionRes.put(e.getKey(), new AuctionRepresentation(auction.getId(), auction.getCompany(), auction.getAmount(), auction.getInterest(), null));
            }
        }

        for(Map.Entry<Integer, Emission> e : company.getEmissionHistory().entrySet()){
            Emission emission = e.getValue();
            emissionRes.put(e.getKey(), new EmissionRepresentation(emission.getId(), emission.getCompany(), emission.getAmount(), emission.getInterest(), emission.getSubscriptions()));
        }

        CompanyRepresentation cr = new CompanyRepresentation(company.getUsername(), auctionRes, emissionRes);

        return Response.status(200).entity(cr).build();
    }

    // Consult available auctions
    @GET
    @Path("/available/auctions")
    public Map<String, AuctionRepresentation> getAvailableAuctions() {
        Map<String, AuctionRepresentation> res = new HashMap<String, AuctionRepresentation>();
        Map<String, Auction> availableAuctions = this.directory.getAvailableAuctions();

        for(Map.Entry<String, Auction> eAuction : availableAuctions.entrySet()){
            Auction auction = eAuction.getValue();
            Map<String, BidRepresentation> bidsRes = new HashMap<String, BidRepresentation>();

            for(Map.Entry<String, Bid> eBid : auction.getBids().entrySet()){
                Bid bid = eBid.getValue();
                bidsRes.put(eBid.getKey(), new BidRepresentation(bid.getInvestor(), bid.getAmount(), bid.getInterest()));
            }
            res.put(eAuction.getKey(), new AuctionRepresentation(auction.getId(), auction.getCompany(), auction.getAmount(), auction.getInterest(), bidsRes));
        }

        return res;
    }

    // Consult available emissions
    @GET
    @Path("/available/emissions")
    public Map<String, EmissionRepresentation> getAvailableEmissions() {
        Map<String, EmissionRepresentation> res = new HashMap<String, EmissionRepresentation>();
        Map<String, Emission> availableEmissions = this.directory.getAvailableEmissions();

        for(Map.Entry<String, Emission> e : availableEmissions.entrySet()){
            Emission emission = e.getValue();
            res.put(e.getKey(), new EmissionRepresentation(emission.getId(), emission.getCompany(), emission.getAmount(), emission.getInterest(), emission.getSubscriptions()));
        }

        return res;
    }

    // Add new available emission
    @POST
    @Path("/add/emission")
    public void putAvailableEmission(EmissionRepresentation emission) {
        this.directory.putAvailableEmission(emission.build());
    }

    // Add new available auction
    @POST
    @Path("/add/auction")
    public void putAvailableAuction(AuctionRepresentation auction) {
        this.directory.putAvailableAuction(auction.build());
    }

    // Add emission to company history
    @POST
    @Path("/end/emission")
    public Response putEmissionHistory(EmissionRepresentation emission) {
        int res = this.directory.putEmissionHistory(emission.build());

        if(res == 0){
            return Response.ok().build();
        }

        return Response.status(404).build();
    }

    // Add auction to company history
    @POST
    @Path("/end/auction")
    public Response putAuctionHistory(AuctionRepresentation auction) {
        int res = this.directory.putAuctionHistory(auction.build());

        if(res == 0){
            return Response.ok().build();
        }

        return Response.status(404).build();
    }
}
