package peerLending.client;

import org.zeromq.ZMQ;
import peerLending.ClientProtos;
import java.io.*;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;


public class Client {
    private String username;
    private String password;
    private String hostname;
    private int port;
    private Socket socket;
    private InputStream in;
    private OutputStream out;
    private BufferedReader reader;
    private ZMQ.Socket subscriber;
    private Map<String, List<String>> enabledNotifications; // Auction/Emission, List<company_name>


    public Client(String hostname, int port) throws IOException {
        this.hostname = hostname;
        this.port = port;

        System.out.println("> Connecting to server...");
        this.socket = new Socket(this.hostname, this.port);
        System.out.println("> Connection accepted!");

        this.in = this.socket.getInputStream();
        this.out = this.socket.getOutputStream();
        this.reader = new BufferedReader(new InputStreamReader(System.in));
    }

    public String getUsername() {
        return this.username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return this.password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public Map<String, List<String>> getEnabledNotifications() {
        return this.enabledNotifications;
    }

    public void setEnabledNotifications(Map<String, List<String>> enabledNotifications) {
        this.enabledNotifications = enabledNotifications;
    }


    public void startClient() throws IOException {
        Thread t = null;

        System.out.println("\n################ LOGIN ################");
        System.out.print("\n> Username: ");
        this.username = this.reader.readLine();
        System.out.print("> Password: ");
        this.password = this.reader.readLine();
        ClientProtos.Authentication auth = ClientProtos.Authentication.newBuilder()
                .setUsername(this.username)
                .setPassword(this.password)
                .build();

        ClientProtos.Message msg = ClientProtos.Message.newBuilder()
                .setAuth(auth)
                .build();

        this.out.write(msg.toByteArray());
        this.out.flush();

        byte [] response = this.receive();
        ClientProtos.Message ans = ClientProtos.Message.parseFrom(response);
        ClientProtos.Result res = ans.getRes();

        Boolean stop = false;
        boolean result = res.getResult();
        String entity = res.getEntity();
        //System.out.println("RESULT: "+result+"\nENTITY: "+entity);
        if (!result)
            System.out.println("ERROR: Authentication failed!");
        else {
            ZMQ.Context context = ZMQ.context(1);
            this.subscriber = context.socket(ZMQ.SUB);
            Subscriber sub = new Subscriber(context, this.subscriber, stop);
            t = new Thread(sub);
            t.start();
            /* TODO: Ir buscar as notificações ativas às exchanges
                 Fazer enable dessas notificações */
            //enableNotifications("auction", enabledNotifications.get("auction"));
            //enableNotifications("emission", enabledNotifications.get("emission"));

            if (entity.equals("investor")) {
                handleInvestor();
            }
            else {
                handleCompany();
            }
        }

        stop = true;
    }


    // INVESTOR
    public void handleInvestor() throws IOException {
        System.out.println("\n############ INVESTOR MENU ############");
        System.out.println("(1) Bid in an auction   |   (2) Subscript fixed fee loan   |   (3) Enable/Disable Notifications   |   (4) Logout");
        System.out.print("\nChoose an option: ");
        String chosenOption = "0";
        while (!chosenOption.equals("1") && !chosenOption.equals("2") && !chosenOption.equals("3") && !chosenOption.equals("4")) {
            chosenOption = this.reader.readLine();
            if (!chosenOption.equals("1") && !chosenOption.equals("2") && !chosenOption.equals("3") && !chosenOption.equals("4"))
                System.out.print("\nInvalid option! Insert a valid one: ");
        }
        if (chosenOption.equals("1"))
            handleBid();
        else if (chosenOption.equals("2"))
            handleSubscription();
        else if (chosenOption.equals("3"))
            handleNotifications("investor");
    }

    public void handleBid() throws IOException {
        System.out.println("\n############# BIDDING MENU ############");
        System.out.println("Complete the following fields.");
        System.out.print("\nCompany: ");
        String company = this.reader.readLine();
        int amount = 1;
        while (amount % 100 != 0) {
            System.out.print("Amount (multiple of 100): ");
            amount = Integer.parseInt(this.reader.readLine());
            if (amount % 100 != 0) {
                System.out.println("ERROR: Invalid amount!");
            }
        }
        System.out.print("Interest: ");
        float interest = Float.parseFloat(this.reader.readLine());

        ClientProtos.Message msg = ClientProtos.Message.newBuilder()
                .setType("Bid")
                .setAmount(amount)
                .setCompany(company)
                .setInvestor(this.username)
                .setInterest(interest)
                .build();

        this.out.write(msg.toByteArray());
        this.out.flush();

        byte [] response = this.receive();
        ClientProtos.Message ans = ClientProtos.Message.parseFrom(response);
        ClientProtos.Result res = ans.getRes();

        boolean result = res.getResult();
        if (!result) {
            System.out.println("\nERROR: Bidding failed!");
        }
        else {
            enableNotification("Auction", company);
            System.out.println("\nSUCCESS: Bidding successful!");
        }
        handleInvestor();
    }

    public void handleSubscription() throws IOException {
        System.out.println("\n########## SUBSCRIPTION MENU ##########");
        System.out.println("Complete the following fields.");
        System.out.print("\nCompany: ");
        String company = this.reader.readLine();
        int amount = 1;
        while (amount % 100 != 0) {
            System.out.print("Amount (multiple of 100): ");
            amount = Integer.parseInt(this.reader.readLine());
            if (amount % 100 != 0) {
                System.out.println("ERROR: Invalid amount!");
            }
        }

        ClientProtos.Message msg = ClientProtos.Message.newBuilder()
                .setType("Subscription")
                .setAmount(amount)
                .setCompany(company)
                .setInvestor(this.username)
                .build();

        this.out.write(msg.toByteArray());
        this.out.flush();

        byte [] response = this.receive();
        ClientProtos.Message ans = ClientProtos.Message.parseFrom(response);
        ClientProtos.Result res = ans.getRes();

        boolean result = res.getResult();
        if (!result) {
            System.out.println("\nERROR: Subscription failed!");
        }
        else {
            enableNotification("Emission", company);
            System.out.println("\nSUCCESS: Subscription successful!");
        }
        handleInvestor();
    }


    // COMPANY
    public void handleCompany() throws IOException {
        System.out.println("\n############# COMPANY MENU ############");
        System.out.println("(1) Create an auction   |   (2) Issue fixed fee   |   (3) Enable/Disable Notifications   |   (4) Logout");
        System.out.print("\nChoose an option: ");
        String chosenOption = "0";
        while (!chosenOption.equals("1") && !chosenOption.equals("2") && !chosenOption.equals("3") && !chosenOption.equals("4")) {
            chosenOption = this.reader.readLine();
            if (!chosenOption.equals("1") && !chosenOption.equals("2") && !chosenOption.equals("3") && !chosenOption.equals("4"))
                System.out.print("\nInvalid option! Insert a valid one: ");
        }
        if (chosenOption.equals("1"))
            handleAuction();
        else if (chosenOption.equals("2"))
            handleEmission();
        else if (chosenOption.equals("3"))
            handleNotifications("company");
    }

    public void handleAuction() throws IOException {
        System.out.println("\n######### CREATE AUCTION MENU #########");
        System.out.println("Complete the following fields.");
        int amount = 1;
        while (amount % 1000 != 0) {
            System.out.print("Amount (multiple of 1000): ");
            amount = Integer.parseInt(this.reader.readLine());
            if (amount % 1000 != 0) {
                System.out.println("ERROR: Invalid amount!");
            }
        }
        System.out.print("Interest: ");
        float interest = Float.parseFloat(this.reader.readLine());

        ClientProtos.Message msg = ClientProtos.Message.newBuilder()
                .setType("Auction")
                .setAmount(amount)
                .setCompany(this.username)
                .setInterest(interest)
                .build();

        this.out.write(msg.toByteArray());
        this.out.flush();

        byte [] response = this.receive();
        ClientProtos.Message ans = ClientProtos.Message.parseFrom(response);
        ClientProtos.Result res = ans.getRes();

        boolean result = res.getResult();
        if (!result) {
            System.out.println("\nERROR: Auction failed!");
        }
        else {
            enableNotification("Auction", this.username);
            System.out.println("\nSUCCESS: Auction successful!");
        }
        handleCompany();
    }

    public void handleEmission() throws IOException {
        System.out.println("\n############ EMISSION MENU ############");
        System.out.println("Complete the following fields.");
        int amount = 1;
        while (amount % 1000 != 0) {
            System.out.print("Amount (multiple of 1000): ");
            amount = Integer.parseInt(this.reader.readLine());
            if (amount % 1000 != 0) {
                System.out.println("ERROR: Invalid amount!");
            }
        }
        System.out.print("Interest: ");
        float interest = Float.parseFloat(this.reader.readLine());

        ClientProtos.Message msg = ClientProtos.Message.newBuilder()
                .setType("Emission")
                .setAmount(amount)
                .setCompany(this.username)
                .setInterest(interest)
                .build();

        this.out.write(msg.toByteArray());
        this.out.flush();

        byte [] response = this.receive();
        ClientProtos.Message ans = ClientProtos.Message.parseFrom(response);
        ClientProtos.Result res = ans.getRes();

        boolean result = res.getResult();
        if (!result) {
            System.out.println("\nERROR: Emission failed!");
        }
        else {
            enableNotification("Emission", this.username);
            System.out.println("\nSUCCESS: Emission successful!");
        }
        handleCompany();
    }


    public void handleNotifications (String entity) throws IOException {
        System.out.println("\n########## NOTIFICATIONS MENU #########");
        System.out.println("(1) Auctions   |   (2) Emissions");
        System.out.print("\nChoose an option: ");
        String chosenOption = checkOption();
        String action;
        if (chosenOption.equals("1"))
            action = "Auction";
        else
            action = "Emission";
        System.out.println("(1) Enable   |   (2) Disable");
        System.out.print("\nChoose an option: ");
        chosenOption = checkOption();
        String status;
        if (chosenOption.equals("1"))
            status = "start";
        else
            status = "cancel";
        int size = 0;
        List<String> companies = new ArrayList<String>();
        while (size > 10) {
            System.out.print("\nInsert companies (separated by a comma, max. 10): ");
            companies = Arrays.asList(this.reader.readLine().split(","));
            size = companies.size();
            if (size > 10) {
                System.out.println("\nERROR: Max number of companies exceeded.");
            }
        }
        for (String company : companies) {
            if (status.equals("start")) {
                enableNotification("End"+action, company);
                enableNotification("Create"+action, company);
                enableNotification("Bid"+action, company);
            }
            else {
                disableNotification("End"+action, company);
                disableNotification("Create"+action, company);
                disableNotification("Bid"+action, company);
            }
        }

        byte [] response = this.receive();
        ClientProtos.Message ans = ClientProtos.Message.parseFrom(response);
        ClientProtos.Result res = ans.getRes();
        boolean result = res.getResult();
        if (!result) {
            System.out.println("\nERROR: Failed to save notifications' options!");
        }
        else {
            System.out.println("\nSUCCESS: Notifications' options saved successfully!");
        }
        if (entity.equals("investor")) {
            handleInvestor();
        }
        else {
            handleCompany();
        }
    }

    public String checkOption() throws IOException {
        String chosenOption = "0";
        while (!chosenOption.equals("1") && !chosenOption.equals("2")) {
            chosenOption = this.reader.readLine();
            if (!chosenOption.equals("1") && !chosenOption.equals("2"))
                System.out.print("\nInvalid option! Insert a valid one: ");
        }
        return chosenOption;
    }

    public void enableNotification (String action, String company) {
        this.subscriber.subscribe(action + ":" + company);
    }

    public void disableNotification (String action, String company) {
        this.subscriber.unsubscribe(action + ":" + company);
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
        Client c = new Client("localhost", 3000);
        while(true) {
            c.startClient();
        }
    }
}
