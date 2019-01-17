package peerLending.exchange;

import peerLending.Auction;
import peerLending.Company;
import peerLending.Emission;
import peerLending.Investor;

import java.io.*;
import java.net.Socket;
import java.util.Map;

public class Exchange {
    private String hostname;
    private int port;
    private Socket socket;
    InputStream in;
    OutputStream out;
    BufferedReader reader;
    private Map<String, Company> companies;
    private Map<String, Investor> investors;
    private Map<String, Auction> availableAuctions;
    private Map<String, Emission> availableEmissions;
    private int auctionCounter;
    private int emissionCounter;

    public Exchange(String hostname, int port) throws IOException {
        this.hostname = hostname;
        this.port = port;
        this.socket = new Socket(this.hostname, this.port);
        this.in = socket.getInputStream();
        this.out = socket.getOutputStream();
        this.reader = new BufferedReader(new InputStreamReader(System.in));
    }
}
