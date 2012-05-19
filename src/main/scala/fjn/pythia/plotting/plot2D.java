package fjn.pythia.plotting;

import java.awt.*;
import java.util.ArrayList;

import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.CombinedDomainXYPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.StandardXYItemRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.ApplicationFrame;
import org.jfree.ui.RefineryUtilities;

import javax.swing.*;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 4/1/12
 * Time: 10:27 AM
 * To change this template use File | Settings | File Templates.
 */
public class plot2D extends ApplicationFrame {
    public plot2D(final String title) {

            super(title);


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

        final CombinedDomainXYPlot plot = new CombinedDomainXYPlot(new NumberAxis("Domain"));
           plot.setGap(10.0);

        final XYItemRenderer renderer1 = new StandardXYItemRenderer();
                final NumberAxis rangeAxis1 = new NumberAxis("x");
        final XYPlot subplot1 = new XYPlot(dataset, null, rangeAxis1, renderer1);
                subplot1.setRangeAxisLocation(AxisLocation.BOTTOM_OR_LEFT);
           // add the subplots...
           plot.add(subplot1, 1);

           plot.setOrientation(PlotOrientation.VERTICAL);


           // return a new chart containing the overlaid plot...

        final JFreeChart chart = new JFreeChart("CombinedDomainXYPlot Demo",
                                         JFreeChart.DEFAULT_TITLE_FONT, plot, true);

                final ChartPanel panel = new ChartPanel(chart, true, true, true, false, true);
                panel.setPreferredSize(new java.awt.Dimension(500, 270));

                setContentPane(panel);

        this.pack();
                RefineryUtilities.centerFrameOnScreen(this);
                this.setVisible(true);

    }
    private XYSeries xySeries = null;

}
