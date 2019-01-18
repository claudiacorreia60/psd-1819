package peerLending.directory.resources;

import org.eclipse.jetty.server.Response;
import peerLending.Company;
import peerLending.directory.Directory;
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
    public int putEmissionHistory(EmissionRepresentation emission) {
        int res = this.directory.putEmissionHistory(emission.build(), emission.getCompany());

        if(res == 0){
            return Response.SC_OK;
        }

        return Response.SC_NOT_FOUND;
    }

    // Add auction to company history
    @POST
    @Path("/end/auction")
    public int putAuctionHistory(AuctionRepresentation auction) {
        int res = this.directory.putAuctionHistory(auction.build(), auction.getCompany());

        if(res == 0){
            return Response.SC_OK;
        }

        return Response.SC_NOT_FOUND;
    }
}
