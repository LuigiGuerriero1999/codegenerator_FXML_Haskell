package com.example.testboy.structures;

import javafx.beans.Observable;
import javafx.collections.ObservableList;

import java.util.Objects;

public class ComboBoxText extends GTKWidget{
    private String comboBoxTextName;

    private boolean entry;
    private ObservableList<?> items;

    private double width;
    private double height;

    public ComboBoxText(String id, Integer id_hash, String name, double layoutX, double layoutY, boolean entry, ObservableList<?> items, double width, double height) {
        super(id, id_hash, makeName(id, id_hash, name+"Container"), layoutX, layoutY);
        this.entry = entry;
        this.items = items;
        this.width = width;
        this.height = height;
        this.comboBoxTextName = makeName(id, id_hash, name);
    }

    public String getComboBoxTextName() {
        return comboBoxTextName;
    }

    public void setComboBoxTextName(String comboBoxTextName) {
        this.comboBoxTextName = comboBoxTextName;
    }

    public boolean isEntry() {
        return entry;
    }

    public void setEntry(boolean entry) {
        this.entry = entry;
    }

    public ObservableList<?> getItems() {
        return items;
    }

    public void setItems(ObservableList<?> items) {
        this.items = items;
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
        String comboBoxTextConstructor = comboBoxTextName + " <- Gtk.comboBoxTextNew \n  " ;
        if (entry) comboBoxTextConstructor = comboBoxTextName + " <- Gtk.comboBoxTextNewWithEntry \n  ";

        StringBuilder comboBoxTextItems = new StringBuilder("");
        for (Object obj : items){
            String item = obj.toString();
            comboBoxTextItems.append("Gtk.comboBoxTextAppendText ").append(comboBoxTextName).append(" ").append("\"").append(item).append("\"").append("\n  ");
        }

        String comboBoxTextContainerBoxName = super.getName();
        String createComboBoxTextContainer = comboBoxTextContainerBoxName + " <- Gtk.boxNew OrientationHorizontal 1\n  ";
        String setButtonContainerProperties = "Gtk.set "+comboBoxTextContainerBoxName+" [Gtk.widgetWidthRequest := "+(int)width+", Gtk.widgetHeightRequest := "+(int)height+"]\n  ";
        String addButtonToContainer = "Gtk.boxPackStart "+comboBoxTextContainerBoxName+" "+comboBoxTextName+" True True 0\n   ";
        String buttonGtkHsCode = comboBoxTextConstructor + comboBoxTextItems + createComboBoxTextContainer + setButtonContainerProperties + addButtonToContainer + "\n  " ;

        return buttonGtkHsCode;
    }
}
