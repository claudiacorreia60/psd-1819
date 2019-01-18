package peerLending.directory.resources;

import peerLending.Directory;
import peerLending.directory.representations.AuctionRepresentation;
import peerLending.directory.representations.EmissionRepresentation;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

@Path("/directory")
@Produces(MediaType.APPLICATION_JSON)
public class DirectoryResource {
    private Directory directory;

    public DirectoryResource() {
        this.directory = new Directory();
    }

    // Consult companies
    @GET
    @Path("/companies")
    public String getCompanies() {
        return this.directory.getCompaniesNames();
    }

    // Consult company history
    @GET
    @Path("/company/{name}")
    public String getCompanyHistory(@PathParam("name") String name) {
        return this.directory.getCompanyHistory(name);
    }

    // Consult available auctions
    @GET
    @Path("/available/auctions")
    public String getAvailableAuctions() {
        return this.directory.getAvailableAuctions();
    }

    // Consult available emissions
    @GET
    @Path("/available/emissions")
    public String getAvailableEmissions() {
        return this.directory.getAvailableEmissions();
    }

    // Add new available emission
    @POST
    @Path("/add/emission")
    public void putAvailableEmission(EmissionRepresentation emission) {
        this.directory.putAvailableEmission(emission.build(), emission.getCompany());
    }

    // Add new available auction
    @POST
    @Path("/add/auction")
    public void putAvailableAuction(AuctionRepresentation auction) {
        this.directory.putAvailableAuction(auction.build(), auction.getCompany());
    }

    // Add emission to company history
    @POST
    @Path("/end/emission")
    public void putEmissionHistory(EmissionRepresentation emission) {
        this.directory.putEmissionHistory(emission.build(), emission.getCompany());
    }

    // Add auction to company history
    @POST
    @Path("/end/auction")
    public void putAuctionHistory(AuctionRepresentation auction) {
        this.directory.putAuctionHistory(auction.build(), auction.getCompany());
    }
}
