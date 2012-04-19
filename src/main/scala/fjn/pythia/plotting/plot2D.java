package fjn.pythia.plotting;

import javax.swing.JFrame;
import javax.swing.JPanel;
import java.awt.*;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;


import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.plot.Marker;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.Marker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.chart.JFreeChart;


/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 4/1/12
 * Time: 10:27 AM
 * To change this template use File | Settings | File Templates.
 */
public class plot2D extends JPanel {
    public plot2D() {
        super();


    }

    XYSeriesCollection dataset = new XYSeriesCollection();

    public void AddCurve(ArrayList<Double> x, ArrayList<Double> y, String title) {
        xySeries = new XYSeries(title);
        if (x.size() != y.size()) {
            throw new ExceptionInInitializerError("size of x and y differs");

        }

        for (int n = 0; n < x.size(); ++n) {
            xySeries.add(x.get(n), y.get(n));
        }


        dataset.addSeries(xySeries);



    }

    public void showPanel()
    {

        JFreeChart chart = ChartFactory.createXYLineChart("xy plot", "normalize coord", "basis", dataset, PlotOrientation.VERTICAL, true, true, false);

                XYPlot plot = (XYPlot) chart.getPlot();
                plot.setDomainGridlineStroke(new BasicStroke(1));

                ChartPanel chartPanel = new ChartPanel(chart);

                add(chartPanel);

                JFrame frame = new JFrame("Chart One");

                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

                frame.getContentPane().add(this, BorderLayout.CENTER);
                frame.pack();
                frame.setVisible(true);

    }
    private XYSeries xySeries = null;

}
