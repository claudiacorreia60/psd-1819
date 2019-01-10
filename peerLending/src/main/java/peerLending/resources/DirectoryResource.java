package peerLending.resources;

import peerLending.Directory;

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
}
