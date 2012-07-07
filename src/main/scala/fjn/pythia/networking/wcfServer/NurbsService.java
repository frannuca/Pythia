package fjn.pythia.networking.wcfServer;

import fjn.pythia.analytics.interpolation.javaInterop.NurbsInterpolator;
import fjn.pythia.analytics.interpolation.javaInterop.NurbsInterpolator1D;
import fjn.pythia.analytics.interpolation.javaInterop.NurbsInterpolator2D;
import fjn.pythia.networking.message.bindings.DMatrix;
import fjn.pythia.networking.message.bindings.DPoint2D;
import fjn.pythia.networking.message.bindings.DVector;

import javax.jws.WebParam;
import javax.jws.WebService;
import java.math.BigDecimal;
import java.util.ArrayList;
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


    public int DropSession(String sessionID) {

        return NurbsServiceSessions.DeleteSession(sessionID);
    }


    @Override
    public String InitializeInterpolator2D(@WebParam(name = "x") DVector x, @WebParam(name = "y") DVector y, @WebParam(name = "z") DMatrix z, @WebParam(name = "orderX") Integer orderX, @WebParam(name = "orderY") Integer orderY) {



        Double[][] zp = new Double[z.getNRows()][];
        for(int i=0;i<z.getNRows();++i)
        {
            
            
            zp[i] = z.getValue().get(i).getValue().toArray(new Double[z.getValue().get(i).getValue().size()]);
        }
        
        NurbsInterpolator2D interpolator = new NurbsInterpolator2D(x.getValue().toArray(new Double[x.getValue().size()]),
                y.getValue().toArray(new Double[y.getValue().size()]),zp,orderX,orderY );
        String id = NurbsServiceSessions.AddSession(interpolator);
        System.out.println(String.format("Initializing Session Id {0}",id));
        return id;
    }

    @Override
    public String InitializeInterpolator1D(@WebParam(name = "x") DVector x, @WebParam(name = "z") DVector z, @WebParam(name = "order") Integer order) {
        
        NurbsInterpolator1D interpolator = new NurbsInterpolator1D(x.getValue(),z.getValue(), order.intValue());
        return NurbsServiceSessions.AddSession(interpolator);
    }
    @Override
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

    public DVector Compute2DMultithreaded(String sessionId, DPoint2D[] points) {
        DVector res = new DVector();
            if (NurbsServiceSessions.reg_.containsKey(sessionId)) {
    
                try {
                    System.out.println(String.format("Computing 2D batch point multithreaded"));
                    NurbsInterpolator2D instance = (NurbsInterpolator2D) NurbsServiceSessions.reg_.get(sessionId);
                    java.lang.Double xs[] = new java.lang.Double[points.length];
                    java.lang.Double ys[] = new java.lang.Double[points.length] ;
                    System.out.println(String.format("Copying the points"));
                    for(int i =0;i<points.length;++i)
                    {
                        xs[i]=points[i].getX();
                        ys[i]=points[i].getY();
                    }
                    System.out.println(String.format("Computing"));
                     Double[] computerVals = instance.compute(xs, ys);
                    System.out.println(String.format("Returning"));
                    for(Double d:computerVals)
                    {
                        res.getValue().add(d);
                    }
                    System.out.println(String.format("Done"));
                    return res;
                } catch (Exception ex) {
                    return null;
                }
            } else {
                return null;
            }
        }
    
    
    @Override
    public DVector Compute2DBatch(@WebParam(name = "session") String sessionId, @WebParam(name = "points") DPoint2D[] points) {
        DVector resV = new DVector();
                
        for(DPoint2D p: points)
        {
            Double r = Compute2D(sessionId, p.getX(), p.getY());
            resV.getValue().add(r);
        }
        return resV;
    }


    @Override
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

    @Override
    public DVector Compute1DBatch(@WebParam(name = "session") String sessionId, @WebParam(name = "points") Double[] points) {
        DVector resV = new DVector();

                for(Double p: points)
                {
                    Double r = Compute1D(sessionId, p);
                    resV.getValue().add(r);
                }
                return resV;
    }

}