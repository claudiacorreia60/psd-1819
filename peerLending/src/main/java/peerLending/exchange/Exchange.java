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

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Timer;

import static java.lang.System.arraycopy;


public class Exchange implements Runnable{
    private int id;
    private int port;
    private Timer timer;
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
        this.timer = new Timer("Timer");
        this.publisher = publisher;
        ZMQ.Context context = ZMQ.context(1);
        this.socket = context.socket(ZMQ.REP);
        this.socket.bind("tcp://localhost:"+port);
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
                byte[] response = this.recv(this.socket);
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

    public byte[] recv(ZMQ.Socket socket) {
        byte[] tmp;
        int len;
        tmp = socket.recv();
        System.out.println("Exchange: Pedido recebido");
        len = tmp.length;
        byte[] response = new byte[len];
        arraycopy(tmp, 0, response, 0, len);
        return response;
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

        if (success) {
            // Notify clients
            synchronized (this.publisher) {
                String notification = "Emission:" + msg.getCompany() + ":" + emission.getAmount() + ":" + emission.getInterest() + ":" + msg.getInvestor() + ":" + msg.getAmount();
                this.publisher.sendNotification(notification);
                notification = "Bid" + notification;
                this.publisher.sendNotification(notification);
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

        if (success) {
            // Notify clients
            synchronized (this.publisher) {
                String notification = "Auction:" + msg.getCompany() + ":" + auction.getAmount() + ":" + auction.getInterest() + ":" + msg.getInvestor() + ":" + msg.getAmount() + ":" + msg.getInterest();
                this.publisher.sendNotification(notification);
                notification = "Bid" + notification;
                this.publisher.sendNotification(notification);
            }
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
            Auction auction = new Auction(this.auctionCounter, msg.getCompany(), msg.getAmount(), msg.getInterest());
            this.availableAuctions.put(msg.getCompany(), auction);
            success = true;

            // Increment auctionCounter
            this.auctionCounter++;

            // Update directory
            sendHTTPRequest("add/auction", auction);

            // Set auction scheduler - 30 seconds
            AuctioneerTask auctioneerTask = new AuctioneerTask(msg.getCompany(), this.companies,"Auction", this.availableAuctions, this.availableEmissions, this.publisher);
            this.timer.schedule(auctioneerTask, 60000);
        }

        if (success) {
            // Notify clients
            synchronized (this.publisher) {
                String notification = "Auction:" + msg.getCompany() + ":" + msg.getAmount() + ":" + msg.getInterest();
                this.publisher.sendNotification(notification);
                notification = "Create" + notification;
                this.publisher.sendNotification(notification);
            }
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
            float interest = getEmissionInterest(msg.getCompany());

            // An emission can be made
            if(interest != -1) {
                success = true;

                // Increment emissionCounter
                this.emissionCounter++;

                // Create new available emission
                Emission emission = new Emission(this.emissionCounter, msg.getCompany(), msg.getAmount(), interest);
                this.availableEmissions.put(msg.getCompany(), emission);

                // Update directory
                sendHTTPRequest("add/emission", emission);

                // Set emission scheduler - 30 seconds
                AuctioneerTask auctioneerTask = new AuctioneerTask(msg.getCompany(), this.companies, "Emission", this.availableAuctions, this.availableEmissions, this.publisher);
                this.timer.schedule(auctioneerTask, 60000);

                // Notify clients
                synchronized (this.publisher) {
                    String notification = "Emission:" + msg.getCompany() + ":" + emission.getAmount() + ":" + emission.getInterest();
                    this.publisher.sendNotification(notification);
                    notification = "Create" + notification;
                    this.publisher.sendNotification(notification);
                }
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

    public float getEmissionInterest (String company) {
        Company c = this.companies.get(company);
        Emission lastEmission = c.getLastEmission();
        Auction lastAuction = c.getLastSuccessfulAuction();

        // It was made at least one emission
        if(lastEmission != null){
            // Last emission unsuccessful
            if(!lastEmission.successful()){
                return lastEmission.getInterest() * (float) 1.1;
            }

            // Last emission successful
            else {
                // Return lowest interest rate between all emissions and last successful auction
                return Float.min(c.getLowestEmissionRate(), lastAuction.highestInterest());
            }
        }

        // There are no auctions in the history
        else {
            // There are no auctions in the history
            if(lastAuction == null){
                return -1;
            }

            // Interest of the last successful auction
            else {
                return lastAuction.highestInterest();
            }
        }
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
        socket.bind("tcp://localhost:6661");
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
