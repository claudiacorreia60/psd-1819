package peerLending.directory;

import peerLending.directory.resources.DirectoryResource;

import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

public class DirectoryApplication extends Application<DirectoryConfiguration>{
    public static void main(String[] args) throws Exception {
        new DirectoryApplication().run(args);
    }

    @Override
    public String getName() { return "Directory"; }

    @Override
    public void initialize(Bootstrap<DirectoryConfiguration> bootstrap) { }

    @Override
    public void run(DirectoryConfiguration configuration,
                    Environment environment) {
        environment.jersey().register(
                new DirectoryResource());
    }
}
