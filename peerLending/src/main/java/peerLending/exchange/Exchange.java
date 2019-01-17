package peerLending.exchange;

import com.google.protobuf.InvalidProtocolBufferException;
import peerLending.*;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class Exchange implements Runnable{
    private int id;
    private int port;
    private ServerSocket server;
    private Socket frontendServer;
    InputStream in;
    OutputStream out;
    private Map<String, Company> companies;
    private Map<String, Auction> availableAuctions;
    private Map<String, Emission> availableEmissions;
    private int auctionCounter;
    private int emissionCounter;

    public Exchange(int port, int id) throws IOException {
        this.port = port;
        this.server = new ServerSocket(this.port);

        // Populate companies
        if(this.id  == 1){
            this.companies.put("apple", new Company("apple"));
            this.companies.put("ibm", new Company("ibm"));
        }
        else if(this.id  == 2){
            this.companies.put("google", new Company("google"));
            this.companies.put("primavera", new Company("primavera"));
        }
        else if(this.id  == 3){
            this.companies.put("edp", new Company("edp"));
            this.companies.put("farfetch", new Company("farfetch"));
        }

        // Initialize structure
        this.availableAuctions = new HashMap<String, Auction>();
        this.availableEmissions = new HashMap<String, Emission>();

        // Initialize counters
        this.auctionCounter = 0;
        this.emissionCounter = 0;
    }

    public void run() {
        try {
            this.frontendServer = this.server.accept();
            this.in = this.frontendServer.getInputStream();
            this.out = this.frontendServer.getOutputStream();

            while(true){
                byte [] response = this.receive();
                ClientProtos.Message msg = ClientProtos.Message.parseFrom(response);

                if("Bid".equals(msg.getType())){
                    // TODO: fazer a handleBid
                    // handleBid(msg);
                }
                else if("Subscription".equals(msg.getType())){
                    // TODO: fazer a handleSubscription
                    // handleSubscription(msg);
                }
                else if("Emission".equals(msg.getType())){
                    handleEmission(msg);
                }
                else if("Auction".equals(msg.getType())){
                    handleAuction(msg);
                }
            }

        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void handleAuction(ClientProtos.Message msg) throws IOException {
        ClientProtos.Result res;

        // Auction already available
        if(this.availableAuctions.containsKey(msg.getCompany())){
            res = ClientProtos.Result.newBuilder()
                    .setResult(false)
                    .build();
        }

        // No auction available
        else {
            // Create new available auction
            Auction auction = new Auction(this.auctionCounter, msg.getAmount(), msg.getInterest());
            this.availableAuctions.put(msg.getCompany(), auction);

            res = ClientProtos.Result.newBuilder()
                    .setResult(true)
                    .build();
        }

        // Increment auctionCounter
        this.auctionCounter++;

        // Reply to frontendServer
        ClientProtos.Message reply = ClientProtos.Message.newBuilder()
                .setType("Auction")
                .setRes(res)
                .build();
        this.out.write(reply.toByteArray());
        this.out.flush();
    }

    public void handleEmission(ClientProtos.Message msg) throws IOException {
        boolean success = false;

        // No emission available
        if(!this.availableEmissions.containsKey(msg.getCompany())){
            // Create new available emission
            int interest = getEmissionInterest(msg.getCompany());

            if(interest != -1) {
                Emission emission = new Emission(this.emissionCounter, msg.getAmount(), interest);
                this.availableEmissions.put(msg.getCompany(), emission);
                success = true;
            }
        }

        // Increment emissionCounter
        this.emissionCounter++;

        // Reply to frontendServer
        ClientProtos.Result res = ClientProtos.Result.newBuilder()
                .setResult(success)
                .build();

        ClientProtos.Message reply = ClientProtos.Message.newBuilder()
                .setType("Emission")
                .setRes(res)
                .build();
        this.out.write(reply.toByteArray());
        this.out.flush();
    }

    public int getEmissionInterest(String company){
        Map<Integer, Auction> allAuctions =  this.companies.get(company).getAuctionHistory();
        Map<Integer, Auction> successfulAuctions = new HashMap<Integer, Auction>();

        for(Map.Entry<Integer, Auction> e : allAuctions.entrySet()){
            // Check if auction was successful
            if(!e.getValue().getBids().isEmpty()){
                successfulAuctions.put(e.getKey(), e.getValue());
            }
        }

        if(successfulAuctions.isEmpty()){
            return -1;
        }

        int maxId = Collections.max(successfulAuctions.keySet());

        Map<String, Bid> bids = successfulAuctions.get(maxId).getBids();

        int maxInterest = 0;

        for(Map.Entry<String, Bid> e : bids.entrySet()){
            if(e.getValue().getInterest() > maxInterest){
                maxInterest = e.getValue().getInterest();
            }
        }

        return maxInterest;
    }

    public byte[] receive(){
        byte[] tmp = new byte[4096];
        int len = 0;
        try {
            len = this.in.read(tmp);
            byte[] response = new byte[len];

            for(int i = 0; i < len; i++)
                response[i] = tmp[i];
            return response;
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void main (String[] args) throws IOException {
        Exchange e1 = new Exchange(5551, 1);
        Exchange e2 = new Exchange(5552, 2);
        Exchange e3 = new Exchange(5553, 3);

        // Initialize threads
        Thread t1 = new Thread(e1);
        Thread t2 = new Thread(e2);
        Thread t3 = new Thread(e3);

        // Start threads
        t1.start();
        t2.start();
        t3.start();
    }
}
