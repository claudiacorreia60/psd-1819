package peerLending;

import org.zeromq.ZMQ;


public class Publisher {
    ZMQ.Context context;
    ZMQ.Socket publisher;


    public Publisher (ZMQ.Context context, ZMQ.Socket publisher) {
        this.context = context;
        this.publisher = publisher;
    }

    public synchronized void sendNotification (String notification) {
        System.out.println("Notification sent: " + notification);
        this.publisher.send(notification);
    }
}
