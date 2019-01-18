package peerLending.client;

import org.zeromq.ZMQ;


public class Subscriber implements Runnable {
    ZMQ.Context context;
    ZMQ.Socket subscriber;


    public Subscriber (ZMQ.Context context, ZMQ.Socket subscriber) {
        this.context = context;
        this.subscriber = subscriber;
    }

    public void run() {
        this.subscriber.connect("tcp://localhost:8888");

        try {
            while (true) {
                String notification = this.subscriber.recvStr();
                String[] splitNotification = notification.split(":");
                if (splitNotification[splitNotification.length-1].equals("End"))
                System.out.println("########### NEW NOTIFICATION ##########");

                System.out.println(notification);
                /*String[] notification = this.subscriber.recvStr().split(":");
                System.out.println(notification[0]);
                System.out.println("> ID: " + notification[1]);
                System.out.println("> Amount: " + notification[2]);
                System.out.println("> Interest: " + notification[3]);
                if (notification[0].equals("auction"))
                    System.out.println("> Bids: " + notification[4]);
                else
                    System.out.println("> Subscriptions: " + notification[4]);*/
            }
        }
        catch (Exception e) {
            System.err.println(e.getMessage());
        }
        finally {
            this.subscriber.close();
            this.context.term();
        }
    }
}
