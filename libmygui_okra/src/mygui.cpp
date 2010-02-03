// mygui.cpp
//
// author: Erik Winkels (aerique@xs4all.nl)
//
// See the LICENSE file in the Okra root directory for more info.

#include "okra-mygui.h"


// Pointers to Common Lisp functions

void (*clfun_mygui_event_mouse_button_click)(Widget*);



// Prototypes

extern "C"
{
    Widget* mygui_find_widget (const char*, bool);
    const char* mygui_get_caption (Widget* widget);
    const char* mygui_get_name (Widget* widget);
    Gui* mygui_initialise (Ogre::RenderWindow*, Ogre::SceneManager*);
    bool mygui_inject_key_press (Gui*, unsigned int, Char);
    bool mygui_inject_key_release (Gui*, unsigned int);
    bool mygui_inject_mouse_move (Gui*, int, int, int);
    bool mygui_inject_mouse_press (Gui*, int, int, int);
    bool mygui_inject_mouse_release (Gui*, int, int, int);
    void mygui_load_layout (const char*);
    void mygui_set_caption (Widget*, const char*);
    void mygui_set_event_mouse_button_click (Widget*);
}


// Functions

Widget* mygui_find_widget (const char* name, bool throw_exception)
{
    // XXX: This is deprecated, use Widget::findWidget in the future with
    // XXX: a list of widgets the should be returned by load_layout.
    return WidgetManager::getInstance().findWidgetT(name, throw_exception);
}


const char* mygui_get_caption (Widget* widget)
{
    return widget->getCaption().asUTF8_c_str();
}


const char* mygui_get_name (Widget* widget)
{
    return widget->getName().c_str();
}


Gui* mygui_initialise (Ogre::RenderWindow* orw, Ogre::SceneManager* osm)
{
    OgrePlatform* platform = new OgrePlatform();
    platform->initialise(orw, osm);
    Gui* gui = new Gui();
    gui->initialise();

    return gui;
}


bool mygui_inject_key_press (Gui* gui, unsigned int key_code, Char code_point)
{
    return gui->injectKeyPress(KeyCode::Enum(key_code), code_point);
}


bool mygui_inject_key_release (Gui* gui, unsigned int key_code)
{
    return gui->injectKeyRelease(KeyCode::Enum(key_code));
}


bool mygui_inject_mouse_move (Gui* gui, int absX, int absY, int absZ)
{
    return gui->injectMouseMove(absX, absY, absZ);
}


bool mygui_inject_mouse_press (Gui* gui, int absX, int absY, int id)
{
    return gui->injectMousePress(absX, absY, MouseButton::Enum(id));
}


bool mygui_inject_mouse_release (Gui* gui, int absX, int absY, int id)
{
    return gui->injectMouseRelease(absX, absY, MouseButton::Enum(id));
}


void mygui_load_layout (const char* name)
{
    LayoutManager::getInstance().load(name);
}


void mygui_set_caption (Widget* widget, const char* caption)
{
    widget->setCaption(caption);
}


void mygui_set_event_mouse_button_click (Widget* widget)
{
    widget->eventMouseButtonClick =
                             newDelegate(clfun_mygui_event_mouse_button_click);
}
