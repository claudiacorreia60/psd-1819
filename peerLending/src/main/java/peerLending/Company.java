package peerLending;

import org.zeromq.ZMQ;
import peerLendingClient.ClientProtos;
import java.io.*;
import java.util.*;


public class Company {
    private String username;
    private String password;
    private InputStream in;
    private OutputStream out;
    private BufferedReader reader;
    private Map<String, List<String>> enabledNotifications; // Auction/Emission, List<company_name>
    private ZMQ.Socket subscriber;
    private Map<Integer, Auction> auctionHistory; // auctions that are over
    private Map<Integer, Emission> emissionHistory; // emissions that are over


    // TODO: Pensar no caso em que o cliente vai abaixo antes das notificações serem guardadas?

    public Company(String username, String password) {
        this.username = username;
        this.password = password;
        this.auctionHistory = new HashMap<Integer, Auction>();
        this.emissionHistory = new HashMap<Integer, Emission>();
    }

    public Company(String username, String password, InputStream in, OutputStream out, BufferedReader reader, ZMQ.Socket subscriber) {
        this.username = username;
        this.password = password;
        this.in = in;
        this.out = out;
        this.reader = reader;
        this.subscriber = subscriber;
        /* TODO: Ir buscar as notificações ativas às exchanges
                 Fazer enable dessas notificações */
        //enableNotifications("auction", enabledNotifications.get("auction"));
        //enableNotifications("emission", enabledNotifications.get("emission"));
        this.auctionHistory = new HashMap<Integer, Auction>();
        this.emissionHistory = new HashMap<Integer, Emission>();
    }

    public void putAuction(Auction auction){
        this.auctionHistory.put(auction.getId(), auction);
    }

    public void putEmission(Emission emission){
        this.emissionHistory.put(emission.getId(), emission);
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public Map<String, List<String>> getEnabledNotifications() {
        return enabledNotifications;
    }

    public void setEnabledNotifications (Map<String, List<String>> enabledNotifications) {
        this.enabledNotifications = enabledNotifications;
    }

    public Map<Integer, Auction> getAuctionHistory() {
        return auctionHistory;
    }

    public void setAuctionHistory(Map<Integer, Auction> auctionHistory) {
        this.auctionHistory = auctionHistory;
    }

    public Map<Integer, Emission> getEmissionHistory() {
        return emissionHistory;
    }

    public void setEmissionHistory(Map<Integer, Emission> emissionHistory) {
        this.emissionHistory = emissionHistory;
    }


    public void handleCompany() throws IOException {
        System.out.println("\n############# COMPANY MENU ############");
        System.out.println("(1) Create an auction   |   (2) Issue fixed fee   |   (3) Enable/Disable Notifications");
        System.out.print("\nChoose an option: ");
        String chosenOption = "0";
        while (!chosenOption.equals("1") && !chosenOption.equals("2") && !chosenOption.equals("3")) {
            chosenOption = this.reader.readLine();
            if (!chosenOption.equals("1") && !chosenOption.equals("2") && !chosenOption.equals("3"))
                System.out.print("\nInvalid option! Insert a valid one: ");
        }
        if (chosenOption.equals("1"))
            handleAuction();
        else if (chosenOption.equals("2"))
            handleEmission();
        else
            handleNotifications();
    }

    public void handleAuction() throws IOException {
        System.out.println("\n######### CREATE AUCTION MENU #########");
        System.out.println("Complete the following fields.");
        System.out.print("Amount: ");
        int amount = Integer.parseInt(this.reader.readLine());
        System.out.print("Interest: ");
        int interest = Integer.parseInt(this.reader.readLine());

        ClientProtos.Message msg = ClientProtos.Message.newBuilder()
                .setType("Auction")
                .setAmount(amount)
                .setCompany(this.username)
                .setInterest(interest)
                .build();

        this.out.write(msg.toByteArray());
        this.out.flush();

        byte [] response = this.recv();
        ClientProtos.Message ans = ClientProtos.Message.parseFrom(response);
        ClientProtos.Result res = ans.getRes();

        boolean result = res.getResult();
        if (!result)
            System.out.println("\nERROR: Auction creation failed!");
        else
            System.out.println("\nSUCCESS: Auction creation successful!");
    }

    public void handleEmission() throws IOException {
        System.out.println("\n############ EMISSION MENU ############");
        System.out.println("Complete the following fields.");
        System.out.print("Amount: ");
        int amount = Integer.parseInt(this.reader.readLine());

        ClientProtos.Message msg = ClientProtos.Message.newBuilder()
                .setType("Emission")
                .setAmount(amount)
                .setCompany(this.username)
                .build();

        this.out.write(msg.toByteArray());
        this.out.flush();

        byte [] response = this.recv();
        ClientProtos.Message ans = ClientProtos.Message.parseFrom(response);
        ClientProtos.Result res = ans.getRes();

        boolean result = res.getResult();
        if (!result)
            System.out.println("\nERROR: Emission failed!");
        else
            System.out.println("\nSUCCESS: Emission successful!");
    }

    public void handleNotifications() throws IOException {
        System.out.println("\n########## NOTIFICATIONS MENU #########");
        System.out.println("(1) Auctions   |   (2) Emissions");
        System.out.print("\nChoose an option: ");
        String chosenOption = checkOption();
        String action;
        if (chosenOption.equals("1"))
            action = "auction";
        else
            action = "emission";
        System.out.println("(1) Enable   |   (2) Disable");
        System.out.print("\nChoose an option: ");
        chosenOption = checkOption();
        String status;
        if (chosenOption.equals("1"))
            status = "start";
        else
            status = "cancel";
        System.out.print("\nInsert companies (separated by a comma): ");
        List<String> companies = Arrays.asList(this.reader.readLine().split(","));

        if (action.equals("auction") && status.equals("start")) {
            enableNotifications(action, companies);
        }
        else if (action.equals("auction") && status.equals("cancel")) {
            disableNotifications(action, companies);
        }
        else if (action.equals("emission") && status.equals("start")) {
            enableNotifications(action, companies);
        }
        else {
            disableNotifications(action, companies);
        }

        byte [] response = this.recv();
        ClientProtos.Message ans = ClientProtos.Message.parseFrom(response);
        ClientProtos.Result res = ans.getRes();
        boolean result = res.getResult();
        if (!result)
            System.out.println("\nERROR: Failed to save notifications' options!");
        else
            System.out.println("\nSUCCESS: Notifications' options saved successfully!");
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

    public void enableNotifications (String action, List<String> companies) {
        for (String company : companies) {
            this.subscriber.subscribe(action + ":" + company);
        }
    }

    public void disableNotifications (String action, List<String> companies) {
        for (String company : companies) {
            this.subscriber.unsubscribe(action + ":" + company);
        }
    }

    public byte[] recv(){
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

    @Override
    public String toString() {
        return "Company{" +
                "username='" + username + '\'' +
                ", auctionHistory=" + auctionHistory.toString() +
                ", emissionHistory=" + emissionHistory.toString() +
                '}';
    }
}
