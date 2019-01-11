package peerLending;

import peerLendingClient.ClientProtos;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;

public class Investor {
    private String username;
    private String password;
    private InputStream in;
    private OutputStream out;
    private BufferedReader reader;

    public Investor(String username, String password) {
        this.username = username;
        this.password = password;
    }

    public Investor(String username, String password, InputStream in, OutputStream out, BufferedReader reader) {
        this.username = username;
        this.password = password;
        this.in = in;
        this.out = out;
        this.reader = reader;
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

    public void handleInvestor() throws IOException {
        System.out.println("\n######### INVESTOR MENU #########");
        System.out.println("(1) Bid in an auction   |   (2) Subscript fixed fee loan   |   (3) Enable/Disable Notifications");
        System.out.print("\nChoose an option: ");
        String chosenOption = "0";
        while (!chosenOption.equals("1") && !chosenOption.equals("2") && !chosenOption.equals("3")) {
            chosenOption = this.reader.readLine();
            if (!chosenOption.equals("1") && !chosenOption.equals("2") && !chosenOption.equals("3"))
                System.out.print("\nInvalid option! Insert a valid one: ");
        }
        if (chosenOption.equals("1"))
            handleBid();
        else if (chosenOption.equals("2"))
            handleSubscription();
        else
            handleNotifications();
    }

    public void handleBid() throws IOException {
        System.out.println("\n######### BIDDING MENU #########");
        System.out.println("Complete the following fields.");
        System.out.print("\nCompany: ");
        String company = this.reader.readLine();
        System.out.print("Amount: ");
        int amount = Integer.parseInt(this.reader.readLine());
        System.out.print("Interest: ");
        int interest = Integer.parseInt(this.reader.readLine());

        ClientProtos.Bid bid = ClientProtos.Bid.newBuilder()
                .setCompany(company)
                .setAmount(amount)
                .setInterest(interest)
                .build();

        bid.writeDelimitedTo(this.out);

        ClientProtos.Result ans = ClientProtos.Result.parseDelimitedFrom(in);
        boolean result = ans.getResult();
        if (!result)
            System.out.println("\nERROR: Bidding failed!");
        else
            System.out.println("\nSUCCESS: Bidding successful!");
    }

    public void handleSubscription() throws IOException {
        System.out.println("\n######### SUBSCRIPTION MENU #########");
        System.out.println("Complete the following fields.");
        System.out.print("\nCompany: ");
        String company = this.reader.readLine();
        System.out.print("Amount: ");
        int amount = Integer.parseInt(this.reader.readLine());

        ClientProtos.Subscription sub = ClientProtos.Subscription.newBuilder()
                .setCompany(company)
                .setAmount(amount)
                .build();

        sub.writeDelimitedTo(this.out);

        ClientProtos.Result ans = ClientProtos.Result.parseDelimitedFrom(in);
        boolean result = ans.getResult();
        if (!result)
            System.out.println("\nERROR: Subscription failed!");
        else
            System.out.println("\nSUCCESS: Subscription successful!");
    }

    public void handleNotifications() throws IOException {
        System.out.println("\n######### NOTIFICATIONS MENU #########");
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
        Iterable<String> companies = Arrays.asList(this.reader.readLine().split(","));

        ClientProtos.Notification notification = ClientProtos.Notification.newBuilder()
                .setStatus(status)
                .setAction(action)
                .addAllCompany(companies)
                .build();

        notification.writeDelimitedTo(this.out);

        ClientProtos.Result ans = ClientProtos.Result.parseDelimitedFrom(in);
        boolean result = ans.getResult();
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
}
