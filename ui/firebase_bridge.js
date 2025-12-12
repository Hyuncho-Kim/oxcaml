import { auth, db, signInAnonymously, onAuthStateChanged, collection, doc, setDoc, getDoc, onSnapshot, query, where, updateDoc, deleteDoc, addDoc, serverTimestamp } from './firebase_config.js';

// Make Firebase functions available globally
window.firebaseAuth = {
  signInAnonymously: () => signInAnonymously(auth),
  onAuthStateChanged: (callback) => onAuthStateChanged(auth, callback),
  getCurrentUser: () => auth.currentUser,
  signOut: () => auth.signOut()
};

window.firestore = {
  db: db,
  collection: collection,
  doc: doc,
  setDoc: setDoc,
  getDoc: getDoc,
  onSnapshot: onSnapshot,
  query: query,
  where: where,
  updateDoc: updateDoc,
  deleteDoc: deleteDoc,
  addDoc: addDoc,
  serverTimestamp: serverTimestamp
};

console.log('Firebase initialized');