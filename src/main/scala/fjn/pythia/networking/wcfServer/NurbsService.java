package fjn.pythia.networking.wcfServer;

import com.eaio.uuid.UUIDGen;
import fjn.pythia.analytics.interpolation.Nurbs1D;
import fjn.pythia.analytics.interpolation.javaInterop.NurbsInterpolator;
import fjn.pythia.analytics.interpolation.javaInterop.NurbsInterpolator1D;
import fjn.pythia.analytics.interpolation.javaInterop.NurbsInterpolator2D;
import fjn.pythia.networking.message.SerializableTest;

import javax.jws.WebMethod;
import javax.jws.WebService;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 5/22/12
 * Time: 7:06 PM
 * To change this template use File | Settings | File Templates.
 */
@WebService(endpointInterface = "fjn.pythia.networking.wcfServer.INurbsService")
public class NurbsService implements INurbsService {

    static class NurbsServiceSessions {

        final static HashMap<String, NurbsInterpolator> reg_ = new HashMap<String, NurbsInterpolator>();

        static String GetSessionID() {
            return UUID.randomUUID().toString();
        }

        static public String AddSession(NurbsInterpolator interpolator) {
            String ticket = GetSessionID();
            reg_.put(ticket, interpolator);
            return ticket;
        }

        static public int DeleteSession(String sessionID) {

            if (reg_.containsKey(sessionID)) {
                reg_.remove(sessionID);
                return 1;
            } else {
                return 0;
            }

        }
    }


    public SerializableTest testPersonId(SerializableTest id)
    {
        SerializableTest res = new SerializableTest();
        res.setPersonId(id.getPersonId()+100);
        return res;
    }
    public String InitializeInterpolator1D(Double[] x, Double[] z, Double order) {
        NurbsInterpolator1D interpolator = new NurbsInterpolator1D(x,z, order);


        return NurbsServiceSessions.AddSession(interpolator);

    }

    public String InitializeInterpolator2D(Double[] x, Double[] y, Double[][] z, Double[] order)  {

        try {
            NurbsInterpolator2D interpolator = new NurbsInterpolator2D(x, y, z, order);

            return NurbsServiceSessions.AddSession(interpolator);
        } catch (Exception ex) {
            return "";
        }

    }


    public int DropSession(String sessionID) {

        return NurbsServiceSessions.DeleteSession(sessionID);
    }


    public Double Compute2D(String sessionId, Double x, Double y) {
        if (NurbsServiceSessions.reg_.containsKey(sessionId)) {

            try {
                NurbsInterpolator2D instance = (NurbsInterpolator2D) NurbsServiceSessions.reg_.get(sessionId);
                return instance.compute(x, y);
            } catch (Exception ex) {
                return -1.0;
            }
        } else {
            return -1.0;
        }
    }


    public Double Compute1D(String sessionId, Double x){
        if (NurbsServiceSessions.reg_.containsKey(sessionId)) {

            try {
                NurbsInterpolator1D instance = (NurbsInterpolator1D) NurbsServiceSessions.reg_.get(sessionId);
                return instance.compute(x);

            } catch (Exception ex) {
               return -1.0;
            }

        } else {
            return -1d;
        }
    }
}