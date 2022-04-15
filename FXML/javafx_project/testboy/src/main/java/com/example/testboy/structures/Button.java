package com.example.testboy.structures;

import com.example.testboy.HelloApplication;

import java.util.Objects;

public class Button extends GTKWidget {
    private String label;
    private String buttonName;

    private double width;
    private double height;

    public Button(String label, double layoutX, double layoutY, double width, double height) {
        this.label = label;
        setLayoutX(layoutX);
        setLayoutY(layoutY);
        this.width = width;
        this.height = height;
    }

    public Button(String id, Integer id_hash, String name, String label, double layoutX, double layoutY, double width, double height) {
        super(id, id_hash, makeName(id,id_hash,name)+"Container", layoutX, layoutY);
        this.label = label;
        this.width = width;
        this.height = height;
        this.buttonName = makeName(id,id_hash,name);
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }


    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height;
    }

    @Override
    public String gtkHsCode(){
        String buttonConstructor = buttonName + " <- Gtk.buttonNew \n  " ;
        if (!Objects.equals(label, "")) buttonConstructor = buttonName + " <- Gtk.buttonNewWithLabel "+"\""+label+"\""+"\n  ";

        String buttonContainerBoxName = super.getName();
        String createButtonContainer = buttonContainerBoxName + " <- Gtk.boxNew OrientationHorizontal 1\n  ";
        String setButtonContainerProperties = "Gtk.set "+buttonContainerBoxName+" [Gtk.widgetWidthRequest := "+(int)width+", Gtk.widgetHeightRequest := "+(int)height+"]\n  ";
        String addButtonToContainer = "Gtk.boxPackStart "+buttonContainerBoxName+" "+buttonName+" True True 0\n   ";
        String buttonGtkHsCode = buttonConstructor + createButtonContainer + setButtonContainerProperties + addButtonToContainer + "\n  " ;

        return buttonGtkHsCode;
    }
}
