import { initializeApp } from 'firebase/app';
import { getAuth, signInAnonymously, onAuthStateChanged } from 'firebase/auth';
import { getFirestore, collection, doc, setDoc, getDoc, onSnapshot, query, where, updateDoc, deleteDoc, addDoc, serverTimestamp } from 'firebase/firestore';

// Firebase config
const firebaseConfig = {
  apiKey: "AIzaSyC6PAdfODVjYyFQm8TFukbgT49iSL-r6XY",
  authDomain: "othello-dd9a2.firebaseapp.com",
  projectId: "othello-dd9a2",
  storageBucket: "othello-dd9a2.firebasestorage.app",
  messagingSenderId: "994823370862",
  appId: "1:994823370862:web:75234437dadb6b0dc24e7f"
};

// Initialize Firebase
const app = initializeApp(firebaseConfig);
const auth = getAuth(app);
const db = getFirestore(app);

export { auth, db, signInAnonymously, onAuthStateChanged, collection, doc, setDoc, getDoc, onSnapshot, query, where, updateDoc, deleteDoc, addDoc, serverTimestamp };