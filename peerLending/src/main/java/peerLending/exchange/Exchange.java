package peerLending.exchange;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.google.protobuf.InvalidProtocolBufferException;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.zeromq.ZMQ;
import peerLending.*;
import java.io.*;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;


/* TODO: Fazer publicação mal façam bid, subscription, auction e emission*/


public class Exchange implements Runnable{
    private int id;
    private int port;
    private ZMQ.Socket socket;
    private Publisher publisher;
    private Map<String, Company> companies;
    private Map<String, Auction> availableAuctions;
    private Map<String, Emission> availableEmissions;
    private int auctionCounter;
    private int emissionCounter;

    public Exchange(int port, int id, Publisher publisher) throws IOException {
        this.port = port;
        this.id = id;
        this.publisher = publisher;
        ZMQ.Context context = ZMQ.context(1);
        this.socket = context.socket(ZMQ.REP);
        socket.bind("tcp://localhost:"+port);
        System.out.println("Running " + this.id + "...");

        // Initialize companies structure
        this.companies = new HashMap<String, Company>();

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

            while(true){
                byte [] response = this.socket.recv();
                System.out.println(response);
                ClientProtos.Message msg = ClientProtos.Message.parseFrom(response);

                if("Bid".equals(msg.getType())){
                    handleBid(msg);
                }
                else if("Subscription".equals(msg.getType())){
                    handleSubscription(msg);
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
        }
    }

    public void handleSubscription (ClientProtos.Message msg) {
        boolean success = false;
        Emission emission = this.availableEmissions.get(msg.getCompany());
        if (emission != null) {
            Map<String, Integer> subscriptions = emission.getSubscriptions();
            int total = 0;
            //total = subscriptions.values().stream().mapToInt(Integer::value).sum();
            for (Integer amount : subscriptions.values()) {
                total += amount;
            }
            if ((total + msg.getAmount()) <= emission.getAmount()) {
                subscriptions.put(msg.getInvestor(), msg.getAmount());
                emission.setSubscriptions(subscriptions);
                success = true;
            }
        }
        // Reply to frontendServer
        ClientProtos.Result res = ClientProtos.Result.newBuilder()
                .setResult(success)
                .build();

        ClientProtos.Message reply = ClientProtos.Message.newBuilder()
                .setType("Subscription")
                .setRes(res)
                .build();
        this.socket.send(reply.toByteArray());
    }

    public void handleBid (ClientProtos.Message msg) {
        boolean success = false;
        Auction auction = this.availableAuctions.get(msg.getCompany());
        if (auction != null) {
            Map<String, Bid> bids = auction.getBids();
            Bid bid = new Bid(msg.getInvestor(), msg.getAmount(), msg.getInterest());
            bids.put(msg.getInvestor(), bid);
            auction.setBids(bids);
            success = true;
        }
        // Reply to frontendServer
        ClientProtos.Result res = ClientProtos.Result.newBuilder()
                .setResult(success)
                .build();

        ClientProtos.Message reply = ClientProtos.Message.newBuilder()
                .setType("Auction")
                .setRes(res)
                .build();
        this.socket.send(reply.toByteArray());
    }

    public void handleAuction(ClientProtos.Message msg) {
        ClientProtos.Result res;
        boolean success = false;

        // No auction available
        if(!this.availableAuctions.containsKey(msg.getCompany())){
            // Create new available auction
            Auction auction = new Auction(this.auctionCounter, msg.getAmount(), msg.getInterest());
            this.availableAuctions.put(msg.getCompany(), auction);
            success = true;

            // Increment auctionCounter
            this.auctionCounter++;

            // Update directory
            sendHTTPRequest("add/auction", auction);

            Handler handler = new Handler(System.currentTimeMillis(), msg.getCompany(), "Auction", this.availableAuctions, this.availableEmissions, this.publisher);
            Thread t = new Thread(handler);
            t.start();
        }

        // Reply to frontendServer
        res = ClientProtos.Result.newBuilder()
                .setResult(success)
                .build();

        ClientProtos.Message reply = ClientProtos.Message.newBuilder()
                .setType("Auction")
                .setRes(res)
                .build();
        this.socket.send(reply.toByteArray());
    }

    public void handleEmission(ClientProtos.Message msg) {
        boolean success = false;

        // No emission available
        if(!this.availableEmissions.containsKey(msg.getCompany())){
            float interest = msg.getInterest();
            float result = getEmissionInterest(msg.getCompany());
            if (interest != result) {
                success = checkEmissionInterest(interest, msg.getCompany());
            }
            else {
                success = true;

                // Increment emissionCounter
                this.emissionCounter++;
            }

            if(success) {
                // Create new available emission
                Emission emission = new Emission(this.emissionCounter, msg.getAmount(), interest);
                this.availableEmissions.put(msg.getCompany(), emission);

                // Update directory
                sendHTTPRequest("add/emission", emission);

                Handler handler = new Handler(System.currentTimeMillis(), msg.getCompany(), "Emission", this.availableAuctions, this.availableEmissions, this.publisher);
                Thread t = new Thread(handler);
                t.start();
            }
        }

        // Reply to frontendServer
        ClientProtos.Result res = ClientProtos.Result.newBuilder()
                .setResult(success)
                .build();

        ClientProtos.Message reply = ClientProtos.Message.newBuilder()
                .setType("Emission")
                .setRes(res)
                .build();
        this.socket.send(reply.toByteArray());
    }

    public float getEmissionInterest(String company){
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

        float maxInterest = 0;

        for(Map.Entry<String, Bid> e : bids.entrySet()){
            if(e.getValue().getInterest() > maxInterest){
                maxInterest = e.getValue().getInterest();
            }
        }

        return maxInterest;
    }

    public boolean checkEmissionInterest (float interest, String company) {
        boolean result = false;
        float maxInterest = 0;
        Map<Integer, Emission> allEmissions = this.companies.get(company).getEmissionHistory();
        Map<Integer, Emission> successfulEmissions = new HashMap<Integer, Emission>();
        int maxId;
        if (!allEmissions.isEmpty()) {
            maxId = Collections.max(allEmissions.keySet());
            for (Map.Entry<Integer, Emission> e : allEmissions.entrySet()) {
                // Check if auction was successful
                if (!e.getValue().getSubscriptions().isEmpty()) {
                    successfulEmissions.put(e.getKey(), e.getValue());
                }
            }
            if (!successfulEmissions.isEmpty()) {
                if (!successfulEmissions.containsKey(maxId)) {
                    maxInterest = (float) (maxInterest * 1.1);
                    if (interest == maxInterest)
                        result = true;
                }
                else {
                    for (Map.Entry<Integer, Emission> e : successfulEmissions.entrySet()) {
                        if (e.getValue().getInterest() == interest) {
                            result = true;
                        }
                    }
                }
            }
        }
        return result;
    }

    public void sendHTTPRequest(String uri, Object obj){
        CloseableHttpClient httpclient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost("http://localhost:8080/directory/" + uri);
        ObjectWriter ow = new ObjectMapper().writer().withDefaultPrettyPrinter();
        String JSON_STRING= null;

        try {
            JSON_STRING = ow.writeValueAsString(obj);
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }

        HttpEntity stringEntity = new StringEntity(JSON_STRING, ContentType.APPLICATION_JSON);
        httpPost.setEntity(stringEntity);
        try {
            httpclient.execute(httpPost);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static void main (String[] args) throws IOException {
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.PUB);
        //socket.bind("tcp://*:" + args[0]);
        Publisher publisher = new Publisher(context, socket);
        Exchange e1 = new Exchange(5551, 1, publisher);
        Exchange e2 = new Exchange(5552, 2, publisher);
        Exchange e3 = new Exchange(5553, 3, publisher);

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
