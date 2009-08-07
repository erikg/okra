// moc.cpp
//
// author: Erik Winkels (aerique@xs4all.nl)
//
// See the LICENSE file in the Okra root directory for more info.

#include "../../../3rd-party/moc-1.0rc1/CollisionTools.h"
#include "okra.h"


// Prototypes

extern "C"
{
    MOC::CollisionTools* moc_initialise (SceneManager*);
    bool moc_raycast_from_camera (MOC::CollisionTools*, RenderWindow*, Camera*,
                                  const okraArray2, okraArray3, MovableObject*&,
                                  float&, const uint32);
}


// Functions

MOC::CollisionTools* moc_initialise (SceneManager* sm)
{
    return new MOC::CollisionTools(sm);
}


bool moc_raycast_from_camera (MOC::CollisionTools* ct, RenderWindow* rw,
                              Camera* cam, const okraArray2 mousecoords,
                              okraArray3 result, MovableObject*& target,
                              float& closest_distance, const uint32 query_mask)
{
    Vector3 resultVector3;

    bool hit = ct->raycastFromCamera(rw, cam,
                                     Vector2(mousecoords[0], mousecoords[1]),
                                     resultVector3, target, closest_distance,
                                     query_mask);
    result[0] = resultVector3[0];
    result[1] = resultVector3[1];
    result[2] = resultVector3[2];

    return hit;
}
