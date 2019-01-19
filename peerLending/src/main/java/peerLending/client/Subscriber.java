package peerLending.client;

import org.zeromq.ZMQ;


public class Subscriber implements Runnable {
    public ZMQ.Context context;
    public ZMQ.Socket subscriber;
    public Boolean stop;


    public Subscriber (ZMQ.Context context, ZMQ.Socket subscriber, Boolean stop) {
        this.context = context;
        this.subscriber = subscriber;
        this.stop = stop;
    }

    public void run() {
        /* TODO: Verificar este endereço */
        this.subscriber.connect("tcp://localhost:6651");

        try {
            while (!stop) {
                String notification = this.subscriber.recvStr();
                String[] splitNotification = notification.split(":");
                if (splitNotification[splitNotification.length-1].equals("Success") || splitNotification[splitNotification.length-1].equals("Failure")) {
                    if (splitNotification[0].equals("Auction") || splitNotification[0].equals("EndAuction"))
                        handleAuctionResult(splitNotification);
                    else
                        handleEmissionResult(splitNotification);
                }
                else if (splitNotification.length == 4) {
                    if (splitNotification[0].equals("CreateAuction") || splitNotification[0].equals("CreateEmission"))
                        handleCreation(splitNotification);
                }
                else {
                    if (splitNotification[0].equals("Auction") || splitNotification[0].equals("BidAuction"))
                        handleBid(splitNotification);
                    else
                        handleSubscription(splitNotification);
                }
            }
        }
        catch (Exception e) {

        } finally {
            this.context.term();
        }
    }

    public void handleAuctionResult (String[] notification) {
        String type = notification[0];
        if (type.equals("EndAuction"))
            System.out.println("########### NEW NOTIFICATION ##########");
        System.out.println("############ AUCTION RESULT ###########");
        System.out.println("\nCompany: "+notification[1]);
        System.out.println("Amount: "+notification[2]);
        System.out.println("Interest: "+notification[3]);
        if (!notification[4].equals("Failure")) {
            System.out.println("Selected bids:");
            for (int i = 4; i < notification.length-1; i+=3) {
                System.out.println("\tInvestor: "+notification[i]);
                System.out.println("\t\tAmount: "+notification[i+1]);
                System.out.println("\t\tInterest: "+notification[i+2]);
            }
        }
        System.out.println("Result: "+notification[notification.length-1]);
        if (type.equals("Auction")) {
            this.subscriber.unsubscribe("Auction:"+notification[1]);
        }
    }

    public void handleEmissionResult (String[] notification) {
        String type = notification[0];
        if (type.equals("EndEmission"))
            System.out.println("########### NEW NOTIFICATION ##########");
        System.out.println("############ EMISSION RESULT ###########");
        System.out.println("\nCompany: "+notification[1]);
        System.out.println("Amount: "+notification[2]);
        System.out.println("Interest: "+notification[3]);
        if (!notification[4].equals("Failure")) {
            System.out.println("Selected subscriptions:");
            for (int i = 4; i < notification.length-1; i+=2) {
                System.out.println("\tInvestor: "+notification[i]);
                System.out.println("\t\tAmount: "+notification[i+1]);
            }
        }
        System.out.println("Result: "+notification[notification.length-1]);
        if (type.equals("Emission")) {
            this.subscriber.unsubscribe("Emission:"+notification[1]);
        }
    }

    public void handleCreation (String[] notification) {
        String type = notification[0].split("Create")[1].toUpperCase();
        System.out.println("########### NEW NOTIFICATION ##########");
        System.out.println("############## NEW "+type+" ############");
        System.out.println("\nCompany: "+notification[1]);
        System.out.println("Amount: "+notification[2]);
        System.out.println("Interest: "+notification[3]);
    }

    public void handleSubscription (String[] notification) {
        String type = notification[0];
        if (type.equals("BidEmission"))
            System.out.println("########### NEW NOTIFICATION ##########");
        System.out.println("########### NEW SUBSCRIPTION ##########");
        System.out.println("\nCompany: "+notification[1]);
        System.out.println("Amount: "+notification[2]);
        System.out.println("Interest: "+notification[3]);
        System.out.println("Subscription:");
        System.out.println("\tInvestor: "+notification[4]);
        System.out.println("\tAmount: "+notification[5]);
    }

    public void handleBid (String[] notification) {
        String type = notification[0];
        if (type.equals("BidAuction"))
            System.out.println("########### NEW NOTIFICATION ##########");
        System.out.println("############### NEW BID ###############");
        System.out.println("\nCompany: "+notification[1]);
        System.out.println("Amount: "+notification[2]);
        System.out.println("Interest: "+notification[3]);
        System.out.println("Bid:");
        System.out.println("\tInvestor: "+notification[4]);
        System.out.println("\tAmount: "+notification[5]);
        System.out.println("\tInterest: "+notification[6]);
    }

}
