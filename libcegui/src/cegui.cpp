// cegui.cpp
//
// author: Erik Winkels (aerique@xs4all.nl)
//
// See the LICENSE file in the Okra root directory for more info.

#include "okra-cegui.h"


// Pointer to Common Lisp function
void (*clfun_cegui_on_event)(const char*, const char*);


// Classes

class EventHandler
{
  public:
    explicit EventHandler (Window* w, const String& e) : m_event(e)
    {
        w->subscribeEvent(e, Event::Subscriber(&EventHandler::handler, this));
    }

  protected:
    bool handler (const EventArgs& ea)
    {
        const WindowEventArgs& we = static_cast<const WindowEventArgs&>(ea);
        String window_name = we.window->getName();

        clfun_cegui_on_event(we.window->getName().c_str(), m_event.c_str());

        return true;
    }

  private:
    const String& m_event;
};


// Prototypes

extern "C"
{
    void cegui_create_font (const char*);
    OgreCEGUIRenderer* cegui_create_renderer (Ogre::RenderWindow*,
                                              Ogre::SceneManager*);
    System* cegui_create_system (OgreCEGUIRenderer*);
    const Image* cegui_get_default_mouse_cursor ();
    Window* cegui_get_window (const char*);
    void cegui_inject_char (utf32);
    void cegui_inject_key_down (unsigned int);
    void cegui_inject_key_up (unsigned int);
    void cegui_inject_mouse_button_down (MouseButton);
    void cegui_inject_mouse_button_up (MouseButton);
    void cegui_inject_mouse_move (float, float);
    void cegui_inject_mouse_position (float, float);
    void cegui_load_scheme (const char*);
    Window* cegui_load_window_layout (const char*);
    void cegui_mouse_cursor_set_image (Image*);
    void cegui_render_gui ();
    void cegui_set_default_font (const char*);
    void cegui_set_default_mouse_cursor (const char*, const char*);
    void cegui_set_gui_sheet (Window*);
    void cegui_set_text (Window*, const char*);
    void cegui_subscribe_event (Window*, const char* event);
}


// Functions

void cegui_create_font (const char* font)
{
    FontManager::getSingleton().createFont(font);
}


OgreCEGUIRenderer* cegui_create_renderer (Ogre::RenderWindow* rw,
                                          Ogre::SceneManager* sm)
{
    return new OgreCEGUIRenderer(rw, Ogre::RENDER_QUEUE_OVERLAY, false, 3000,
                                 sm);
}


System* cegui_create_system (OgreCEGUIRenderer* ocr)
{
    return new System(ocr);
}


const Image* cegui_get_default_mouse_cursor ()
{
    return System::getSingleton().getDefaultMouseCursor();
}


Window* cegui_get_window (const char* w)
{
    return WindowManager::getSingleton().getWindow(w);
}


void cegui_inject_char (utf32 code_point)
{
    System::getSingleton().injectChar(code_point);
}


void cegui_inject_key_down (unsigned int key_code)
{
    System::getSingleton().injectKeyDown(key_code);
}


void cegui_inject_key_up (unsigned int key_code)
{
    System::getSingleton().injectKeyUp(key_code);
}


void cegui_inject_mouse_button_down (MouseButton mb)
{
    System::getSingleton().injectMouseButtonDown(mb);
}


void cegui_inject_mouse_button_up (MouseButton mb)
{
    System::getSingleton().injectMouseButtonUp(mb);
}


void cegui_inject_mouse_move (float dx, float dy)
{
    System::getSingleton().injectMouseMove(dx, dy);
}


void cegui_inject_mouse_position (float x, float y)
{
    System::getSingleton().injectMousePosition(x, y);
}


void cegui_load_scheme (const char* scheme)
{
    SchemeManager::getSingleton().loadScheme(scheme);
}


Window* cegui_load_window_layout (const char* layout)
{
    return WindowManager::getSingleton().loadWindowLayout(layout);
}


void cegui_mouse_cursor_set_image (Image* image)
{
    MouseCursor::getSingleton().setImage(image);
}


void cegui_render_gui ()
{
    System::getSingleton().renderGUI();
}


void cegui_set_default_font (const char* font)
{
    System::getSingleton().setDefaultFont(font);
}


void cegui_set_default_mouse_cursor (const char* look, const char* arrow)
{
    System::getSingleton().setDefaultMouseCursor(look, arrow);
}


void cegui_set_gui_sheet (Window* sheet)
{
    System::getSingleton().setGUISheet(sheet);
}


void cegui_set_text (Window* window, const char* text)
{
    window->setText(text);
}


void cegui_subscribe_event (Window* window, const char* event)
{
    // XXX: the string is never deallocated (for now), so this leaks memory
    // From C++ itself you would pass PushButton::EventClicked instead of *e,
    // but I'm not sure about the best way to handle this.  I find a huge
    // lookup table not very appealing.
    const String* e = new String(event);
    new EventHandler(window, *e);
}
